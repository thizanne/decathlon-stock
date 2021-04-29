open Async_smtp

module Lwt_monad = Monad.Make (struct
    open Lwt.Infix
    type 'a t = 'a Lwt.t
    let bind x ~f = x >>= f
    let return x = Lwt.return x
    let map = `Custom (fun x ~f -> x >|= f)
  end)

open Lwt_monad.Let_syntax

let run_in_async f =
  Lwt_preemptive.detach begin
    fun () ->
      Async.Thread_safe.run_in_async_wait f
      |> Result.map_error ~f:Error.of_exn
      |> Result.join
  end ()

module Stock = struct
  (* In last versions, stock has become more complicated than just
     integers. Better have a clean dedicated module. *)

  type availability =
    (* From [https://schema.org/ItemAvailability] *)
    | In_stock
    | Out_of_stock
    | Limited_availability of int

  type size = string

  type stock = (size, availability) List.Assoc.t

  let limited_or_out number =
    if number = 0
    then Out_of_stock
    else Limited_availability number

  let parse_availability_exn string =
    let in_sentences = [
      "https://schema.org/OutOfStock", Out_of_stock;
      "https://schema.org/InStock", In_stock;
    ] in
    let string = String.strip string in
    match List.Assoc.find ~equal:String.Caseless.equal in_sentences string with
    | Some result -> result
    | None -> limited_or_out @@ int_of_string string

  let is_empty = function
    | Out_of_stock -> true
    | Limited_availability x -> x = 0
    | In_stock -> false

  let is_not_empty x =
    not (is_empty x)

  let is_empty_for_size size stocks =
    match List.Assoc.find ~equal:String.Caseless.equal stocks size with
    | None -> true
    | Some stock -> is_empty stock

  let is_not_empty_for_size size stocks =
    not (is_empty_for_size size stocks)

  let pp_availability ppf = function
    | In_stock -> Fmt.string ppf "En stock"
    | Out_of_stock -> Fmt.string ppf "En rupture de stock"
    | Limited_availability x -> Fmt.int ppf x

  let pp_size_stock ppf (size, stock) =
    Format.fprintf ppf "%s : %a" size pp_availability stock

  let pp =
    Fmt.list @@ Fmt.box pp_size_stock

  let pp_with_name ppf (item_name, stock) =
    Format.fprintf ppf "Stock pour %s :@ @[<v>%a@]@."
      item_name pp stock

end

module Arg = struct

  open Cmdliner

  let url =
    let doc = "Url of the product to get stock info." in
    let info_ = Arg.info [] ~doc ~docv:"URL" in
    Arg.(required & pos ~rev:true 0 (some string) None & info_)

  let sizes =
    let doc = "Sizes to get stock of. Case-insensitive. If not provided, all will be listed." in
    let info_ = Arg.info ["sizes"] ~doc ~docv:"SIZES" in
    Arg.(value & (opt (some (list string)) None) & info_)

  let mail =
    let doc = "Mail address to which a notification will be sent if one size has stock." in
    let info_ = Arg.info ["mail"] ~docv:"MAIL" ~doc in
    Arg.(value & opt (some string) None & info_)

  let period =
    let default = 5 in
    let doc = "Period in minutes between consecutive checks." in
    let info_ = Arg.info ["period"] ~docv:"MINUTES" ~doc in
    Arg.(value & opt int default & info_)

end

module Notify = struct

  let send_mail ~mail ~item ~url stock =
    let%bind () =
      Lwt_fmt.printf "@[Sending mail...@]@." in
    let subject =
      Format.sprintf "Stock disponible pour %s !" item in
    let content =
      Format.asprintf "@[<v>@[<v>%a@]@ %s@]" Stock.pp_with_name (item, stock) url in
    run_in_async @@ fun () ->
    Simplemail.send
      ~to_:[Email_address.of_string_exn mail]
      ~subject
      (Simplemail.Content.text_utf8 content)

  let if_needed ~mail ~item ~url ~old_stock new_stock =
    let cutoff =
      List.exists
        ~f:(fun (size, stock) ->
            (* Notify iff some size is in stock only on the new one *)
            Stock.is_not_empty stock &&
            Stock.is_empty_for_size size old_stock)
        new_stock in
    match mail with
    | Some mail when cutoff ->
      send_mail ~mail ~item ~url new_stock >>|
      Or_error.ok_exn
    | _ -> Lwt.return ()

end

module Parse = struct

  (* Utilities *)

  let list_ x = `List x

  let as_singleton_exn = function
    (* Utility function for parsing *)
    | [x] -> x
    | _ -> failwith "as_singleton"

  (* Parsing the html soup *)

  let extract_end_json soup =
    soup
    |> Soup.R.select_one {|script#__dkt|}
    |> Soup.R.leaf_text
    |> Yojson.Safe.from_string

  let extract_head_json soup =
    soup
    |> Soup.R.select_one "head"
    |> Soup.select {|script[type='application/ld+json']|}
    |> Soup.R.first
    |> Soup.R.leaf_text
    |> Yojson.Safe.from_string

  let parse_item_name soup =
    (* Could also be done from the json, but works. *)
    soup
    |> Soup.R.select_one {|h1.title|}
    |> Soup.R.leaf_text

  (* Parse the json *)

  let data_member_of_supermodel value =
    (* Returns the data field (in Some) if value has type "Supermodel", else None *)
    let type_ = Yojson.Safe.Util.(to_string @@ member "type" value) in
    if String.Caseless.(type_ = "Supermodel")
    then Some (Yojson.Safe.Util.member "data" value)
    else None

  let extract_skuids_to_size json =
    let open Yojson.Safe.Util in
    json
    |> member "_ctx"
    |> member "data"
    |> to_list
    |> filter_map data_member_of_supermodel
    |> as_singleton_exn
    |> member "models"
    |> map (member "skus")
    |> to_list
    |> flatten
    |> List.map ~f:(fun sku -> member "skuId" sku, member "size" sku)
    |> List.map ~f:(Tuple2.map ~f:to_string)

  let extract_skuid_stocks json =
    let open Yojson.Safe.Util in
    json
    |> member "offers"
    |> index 0 (* offers seems to be an array of size 1? *)
    |> to_list
    |> List.map ~f:(fun offer -> member "sku" offer, member "availability" offer)
    |> List.map ~f:(Tuple2.map ~f:to_string)
    |> List.map ~f:(Tuple2.map_snd ~f:Stock.parse_availability_exn)

  let combine_size_and_stock ~sizes:skuid_to_size ~stocks:skuid_to_stock =
    List.map
      ~f:(fun (skuid, stock) ->
          let size =
            List.Assoc.find_exn
              ~equal:String.equal
              skuid_to_size
              skuid in
          (size, stock))
      skuid_to_stock

  let parse_stock soup =
    let sizes =
      extract_skuids_to_size @@
      extract_end_json soup in
    let stocks =
      extract_skuid_stocks @@
      extract_head_json soup in
    combine_size_and_stock ~sizes ~stocks

end

let get_soup url =
  Cohttp_lwt_unix.Client.get @@ Uri.of_string url >>= fun (_, body) ->
  Cohttp_lwt.Body.to_string body >>|
  Soup.parse

let filter_stock specified_sizes stock =
  match specified_sizes with
  | None -> stock (* All sizes if no specification *)
  | Some sizes ->
    List.filter
      ~f:(fun (size, _) -> List.mem ~equal:String.Caseless.equal sizes size)
      stock

let fetch url sizes =
  let%bind soup = get_soup url in
  let item = Parse.parse_item_name soup in
  let stock = Parse.parse_stock soup in
  let stock = filter_stock sizes stock in
  Lwt.return (item, stock)

let fetch_and_inform ~mail ~url ~sizes ~old_stock =
  let%bind (item, new_stock) =
    fetch url sizes in
  let%bind () =
    Lwt_fmt.printf "%a@." Stock.pp_with_name (item, new_stock) in
  let%bind () =
    Notify.if_needed ~mail ~item ~url ~old_stock new_stock in
  Lwt.return new_stock

let rec loop ~mail ~url ~sizes ~period old_stock =
  let period_seconds = float period *. 60. in
  let%bind new_stock =
    fetch_and_inform ~mail ~url ~sizes ~old_stock in
  let%bind () = Lwt_unix.sleep period_seconds in
  loop ~mail ~url ~sizes ~period new_stock >>|
  never_returns

let main url sizes mail period =
  (* Start with fully empty stock *)
  Lwt_main.run @@
  loop ~mail ~url ~sizes ~period []

let main_term : never_returns Cmdliner.Term.t =
  Cmdliner.Term.(const main $ Arg.url $ Arg.sizes $ Arg.mail $ Arg.period)

let cmd_info =
  let open Cmdliner in
  let doc = "Check stock for a Decathlon product." in
  let exits = Term.default_exits in
  Term.info "decathlon-stock" ~version:"0.3.0" ~doc ~exits

let () =
  Cmdliner.Term.(exit @@ eval (main_term, cmd_info))

open Async_smtp

module Lwt_monad = Monad.Make (struct
    open Lwt.Infix
    type 'a t = 'a Lwt.t
    let bind x ~f = x >>= f
    let return x = Lwt.return x
    let map = `Custom (fun x ~f -> x >|= f)
  end)

open Lwt_monad.Let_syntax

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

let get_soup url =
  Cohttp_lwt_unix.Client.get @@ Uri.of_string url >>= fun (_, body) ->
  Cohttp_lwt.Body.to_string body >>|
  Soup.parse

let parse_item_name soup =
  soup
  |> Soup.R.select_one {|h1.title--main|}
  |> Soup.R.leaf_text

let parse_size_node node =
  Soup.R.select_one {|.sizes__info|} node
  |> Soup.R.leaf_text,
  Soup.R.select_one {|.sizes__stock|} node
  |> Soup.R.leaf_text

let parse_stock soup =
  soup
  |> Soup.select {|.sizes__size|}
  |> Soup.to_list
  |> List.map ~f:parse_size_node

let filter_stock specified_sizes stock =
  match specified_sizes with
  | None -> stock (* All sizes if no specification *)
  | Some sizes ->
    List.filter
      ~f:(fun (size, _) -> List.mem ~equal:String.Caseless.equal sizes size)
      stock

let pp_size_stock fmt (size, stock) =
  Format.fprintf fmt "%s : %s" size stock

let pp_all_stock =
  Fmt.list @@ Fmt.box pp_size_stock

let pp_item_stock fmt (item, stock) =
  Format.fprintf fmt "Stock pour %s :@ @[<v>%a@]"
    item pp_all_stock stock

let run_in_async f =
  Lwt_preemptive.detach begin
    fun () ->
      Async.Thread_safe.run_in_async_wait f
      |> Result.map_error ~f:Error.of_exn
      |> Result.join
  end ()

let fetch url sizes =
  let%bind soup = get_soup url in
  let item = parse_item_name soup in
  let stock = parse_stock soup in
  let stock = filter_stock sizes stock in
  Lwt.return (item, stock)

let send_mail ~mail ~item ~url stock =
  let%bind () = Lwt_fmt.printf "@[Sending mail...@]@." in
  let subject = Format.sprintf "Stock disponible pour %s !" item in
  let content = Format.asprintf "@[<v>@[<v>%a@]@ %s@]" pp_item_stock (item, stock) url in
  run_in_async @@ fun () ->
  Simplemail.send
    ~to_:[Email_address.of_string_exn mail]
    ~subject
    (Simplemail.Content.text_utf8 content)

let stock_has_size stock size =
  match List.Assoc.find ~equal:String.Caseless.equal stock size with
  | None
  | Some "0" -> false
  | Some _ -> true

let notify ~mail ~item ~url ~old_stock new_stock =
  let cutoff =
    List.exists
      ~f:(fun (size, stock) ->
          (* Notify iff some size is in stock only on the new one *)
          String.(stock <> "0") &&
          not @@ stock_has_size old_stock size)
      new_stock in
  match mail with
  | Some mail when cutoff ->
    send_mail ~mail ~item ~url new_stock >>|
    Or_error.ok_exn
  | _ -> Lwt.return ()

let fetch_and_inform ~mail ~url ~sizes ~old_stock =
  let%bind (item, new_stock) =
    fetch url sizes in
  let%bind () =
    Lwt_fmt.printf "%a@." pp_item_stock (item, new_stock) in
  let%bind () =
    notify ~mail ~item ~url ~old_stock new_stock in
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
  Term.info "decathlon-stock" ~version:"0.2.0" ~doc ~exits

let () =
  Cmdliner.Term.(exit @@ eval (main_term, cmd_info))

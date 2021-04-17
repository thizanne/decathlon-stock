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
    let info_ = Arg.info ["s"; "sizes"] ~doc ~docv:"SIZES" in
    Arg.(value & (opt (some (list string)) None) & info_)

  let mail =
    let doc = "Mail address to which a notification will be sent if one size has stock." in
    let info_ = Arg.info ["mail"] ~docv:"MAIL" ~doc in
    Arg.(value & opt (some string) None & info_)

end

let stock_is_empty stock =
  List.for_all
    ~f:(fun (_, one) -> String.equal one "0")
    stock

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

let sendmail item stock url mail =
  let subject = Format.sprintf "Stock disponible pour %s !" item in
  let content = Format.asprintf "@[<v>@[<v>%a@]@ %s@]" pp_item_stock (item, stock) url in
  run_in_async @@ fun () ->
  Simplemail.send
    ~to_:[Email_address.of_string_exn mail]
    ~subject
    (Simplemail.Content.text_utf8 content)

let main url sizes mail : unit =
  Lwt_main.run @@
  let%bind soup = get_soup url in
  let item = parse_item_name soup in
  let stock = parse_stock soup in
  let stock = filter_stock sizes stock in
  let%bind () =
    match mail with
    | Some mail when not (stock_is_empty stock) ->
      sendmail item stock url mail >>|
      Or_error.ok_exn
    | _ -> Lwt.return ()
  in
  Lwt_fmt.printf "%a@." pp_item_stock (item, stock)

let main_term : unit Cmdliner.Term.t =
  Cmdliner.Term.(const main $ Arg.url $ Arg.sizes $ Arg.mail)

let cmd_info =
  let open Cmdliner in
  let doc = "Check stock for a Decathlon product." in
  let exits = Term.default_exits in
  Term.info "decathlon-stock" ~version:"0.1.0" ~doc ~exits

let () =
  Cmdliner.Term.(exit @@ eval (main_term, cmd_info))

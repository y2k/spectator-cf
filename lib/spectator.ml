open Js_of_ocaml
open Promise.Syntax

module Handler = struct
  module StringMap = Map.Make (String)

  type state = { subs : string list StringMap.t }

  let handle_command () =
    let state = ref { subs = StringMap.empty } in
    fun (user_id : string) (msg : string) ->
      match String.split_on_char ' ' msg with
      | [ "/ls" ] ->
          let subs =
            !state.subs |> StringMap.find_opt user_id
            |> Option.value ~default:[]
          in
          if subs = [] then "No subscriptions"
          else
            subs |> List.fold_left (Printf.sprintf "%s\n- %s") "Subscriptions:"
      | [ "/add"; url ] ->
          let subs =
            !state.subs |> StringMap.find_opt user_id
            |> Option.value ~default:[]
          in
          let subs = url :: subs in
          state := { subs = StringMap.add user_id subs !state.subs };
          "Subscription added"
      | _ -> failwith "???"
end

type message_from = { id : int } [@@deriving yojson { strict = false }]

type message = { text : string; from : message_from }
[@@deriving yojson { strict = false }]

type message_upd = { message : message } [@@deriving yojson { strict = false }]

let handler = Handler.handle_command ()

let send_to_user user_id message =
  let tg_token = Js.Unsafe.global ##. TG_TOKEN_ in
  let* response =
    Js.Unsafe.fun_call Js.Unsafe.global##.fetch
      [|
        Js.Unsafe.inject
          ("https://api.telegram.org/bot" ^ tg_token ^ "/sendMessage");
        `Assoc
          [
            ( "body",
              `String
                (`Assoc
                   [ ("chat_id", `String user_id); ("text", `String message) ]
                |> Yojson.pretty_to_string) );
            ("method", `String "POST");
            ("headers", `Assoc [ ("Content-Type", `String "application/json") ]);
          ]
        |> Yojson.to_string |> Js.string |> Json.unsafe_input;
      |]
  in
  Promise.return response

let handle_fetch request =
  let* text = request##text in
  let json = text |> Yojson.Safe.from_string in
  (* json |> Yojson.Safe.pretty_to_string |> print_endline; *)
  (* json |> Yojson.Safe.show |> print_endline; *)
  let msg_promise =
    json |> message_upd_of_yojson
    |> Result.map (fun x ->
           handler (string_of_int x.message.from.id) x.message.text
           |> send_to_user (string_of_int x.message.from.id))
  in
  let* _r =
    match msg_promise with Ok x -> x | Error _e -> Promise.return Ojs.null
  in
  Promise.return ()

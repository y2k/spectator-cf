open Effects

(* Move to common *)
let send_telegram_mesage user_id response_msg =
  Effects.fetch
    ( "https://api.telegram.org/bot~TG_TOKEN~/sendMessage",
      `Assoc
        [
          ( "body",
            `String
              (`Assoc
                 [
                   ("chat_id", `String user_id); ("text", `String response_msg);
                 ]
              |> Yojson.Safe.to_string) );
          ("method", `String "POST");
          ("headers", `Assoc [ ("Content-Type", `String "application/json") ]);
        ] )

let handle_ls_command user_id =
  let open Io.Syntax in
  let* new_subs, subs =
    Io.combine2
      (Effects.query
         ( "SELECT * FROM new_subscriptions WHERE content->>'user_id' = ?",
           [ user_id ] ))
      (Effects.query
         ( "SELECT * FROM subscriptions WHERE content->>'user_id' = ?",
           [ user_id ] ))
  in
  let response_msg =
    match new_subs @ subs with
    | [] -> "No subscriptions"
    | subs ->
        subs
        |> List.map (fun x ->
               let module U = Yojson.Safe.Util in
               let json =
                 U.member "content" x |> U.to_string |> Yojson.Safe.from_string
               in
               (json |> U.member "url" |> U.to_string)
               ^ " ["
               ^ (json |> U.member "type" |> U.to_string_option
                 |> Option.map (function
                      | "b18036ba-0838-437a-8925-7524cf8b07b9" -> "RSS"
                      | x -> x)
                 |> Option.value ~default:"…")
               ^ "]")
        |> List.fold_left (Printf.sprintf "%s\n- %s") "Subscriptions:"
  in
  send_telegram_mesage user_id response_msg |> Io.ignore

let handle_add_command user_id url =
  let response_msg = "Subscription added" in
  let content =
    `Assoc [ ("url", `String url); ("user_id", `String user_id) ]
    |> Yojson.Safe.to_string
  in
  let f1 =
    Effects.query
      ("INSERT INTO new_subscriptions (content) VALUES (?)", [ content ])
  in
  let f2 = send_telegram_mesage user_id response_msg in
  Io.combine2 f1 f2 |> Io.ignore

type message_from = { id : int } [@@deriving yojson { strict = false }]

type message = { text : string; from : message_from }
[@@deriving yojson { strict = false }]

type message_upd = { message : message } [@@deriving yojson { strict = false }]

let handle =
  let open Io.Syntax in
  let* message = Effects.read_const in
  message |> Yojson.Safe.from_string |> message_upd_of_yojson |> function
  | Ok msg_model -> (
      let user_id = string_of_int msg_model.message.from.id in
      match String.split_on_char ' ' msg_model.message.text with
      | [ "/ls" ] -> handle_ls_command user_id
      | [ "/add"; url ] -> handle_add_command user_id url
      | _ ->
          send_telegram_mesage user_id
            "/ls - list of subscription\n/add - add new subscription"
          |> Io.ignore)
  | Error _e -> failwith __LOC__

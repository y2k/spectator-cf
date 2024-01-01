open Effects

let send_telegram_mesage user_id response_msg =
  Db.fetch
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
  let+ new_subs, subs =
    Io.combine2
      (Db.query
         ("SELECT * FROM new_subscriptions WHERE user_id = ?", [ user_id ]))
      (Db.query ("SELECT * FROM subscriptions WHERE user_id = ?", [ user_id ]))
  in
  let response_msg =
    match new_subs @ subs with
    | [] -> "No subscriptions"
    | subs ->
        subs
        |> List.map (fun x ->
               Yojson.Safe.Util.member "content" x
               |> Yojson.Safe.Util.to_string |> Yojson.Safe.from_string
               |> Yojson.Safe.Util.member "url"
               |> Yojson.Safe.Util.to_string)
        |> List.fold_left (Printf.sprintf "%s\n- %s") "Subscriptions:"
  in
  send_telegram_mesage user_id response_msg |> Io.ignore

let handle_add_command user_id url =
  let response_msg = "Subscription added" in
  let content = `Assoc [ ("url", `String url) ] |> Yojson.Safe.to_string in
  let f1 =
    Db.query
      ( "INSERT INTO new_subscriptions (user_id, content) VALUES (?, ?)",
        [ user_id; content ] )
  in
  let f2 = send_telegram_mesage user_id response_msg in
  Io.combine2 f1 f2 |> Io.ignore |> Io.pure

type message_from = { id : int } [@@deriving yojson { strict = false }]

type message = { text : string; from : message_from }
[@@deriving yojson { strict = false }]

type message_upd = { message : message } [@@deriving yojson { strict = false }]

let handle_message message =
  let msg_model = message |> Yojson.Safe.from_string |> message_upd_of_yojson in
  match msg_model with
  | Ok msg_model -> (
      let user_id = string_of_int msg_model.message.from.id in
      match String.split_on_char ' ' msg_model.message.text with
      | [ "/ls" ] -> handle_ls_command user_id
      | [ "/add"; url ] -> handle_add_command user_id url
      | _ ->
          send_telegram_mesage user_id
            "/ls - list of subscription\n/add - add new subscription"
          |> Io.ignore |> Io.pure)
  | Error _e -> failwith __LOC__

let handle_fetch request env =
  let open Promise.Syntax in
  let* (text : string) = request##text in
  let effect = handle_message text in
  Promise.make (fun ~resolve ~reject:_ ->
      let w : Io.world =
        { perform = Io.unhandled }
        |> RealEffectHandlers.attach_db_effect env
        |> RealEffectHandlers.attach_fetch_effect env
        |> RealEffectHandlers.attach_log_effect
      in
      effect.f w (fun w e2 -> e2.f w (fun _ () -> resolve ())))

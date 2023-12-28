open Effects

let send_telegram_mesage user_id response_msg =
  Effect.perform
    (Fetch
       ( "https://api.telegram.org/bot~TG_TOKEN~/sendMessage",
         `Assoc
           [
             ( "body",
               `String
                 (`Assoc
                    [
                      ("chat_id", `String user_id);
                      ("text", `String response_msg);
                    ]
                 |> Yojson.Safe.to_string) );
             ("method", `String "POST");
             ("headers", `Assoc [ ("Content-Type", `String "application/json") ]);
           ] ))

let handle_ls_command user_id =
  let open Io.Syntax in
  let+ new_subs, subs =
    Io.combine2
      (Effect.perform
         (QueryDbFx
            ("SELECT * FROM new_subscriptions WHERE user_id = ?", [ user_id ])))
      (Effect.perform
         (QueryDbFx
            ("SELECT * FROM subscriptions WHERE user_id = ?", [ user_id ])))
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
    Effect.perform
      (QueryDbFx
         ( "INSERT INTO new_subscriptions (user_id, content) VALUES (?, ?)",
           [ user_id; content ] ))
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

module RealEffectHandlers = struct
  open Effect.Deep
  open Js_of_ocaml

  let execute_sql env (sql : string) (params : string list) :
      Yojson.Safe.t list Promise.t =
    let open Promise.Syntax in
    let+ result =
      (Js.Unsafe.meth_call
         (env ##. DB##prepare sql)
         "bind"
         (params |> List.map Js.Unsafe.inject |> Array.of_list))##run
    in
    result##.results |> Json.output |> Js.to_string |> Yojson.Safe.from_string
    |> function
    | `List xs -> xs
    | x -> [ x ]

  let fix_url env url =
    let token = env ##. TG_TOKEN_ in
    url |> String.split_on_char '~'
    |> List.map (function "TG_TOKEN" -> token | x -> x)
    |> List.fold_left ( ^ ) ""

  let rec with_effect : 'a 'b. _ -> ('a -> 'b) -> 'a -> 'b =
   fun env f x ->
    try_with f x
      {
        effc =
          (fun (type a) (eff : a Effect.t) ->
            match eff with
            | QueryDbFx ((sql, params) as p) ->
                Some
                  (fun (k : (a, _) continuation) ->
                    continue k
                      {
                        f =
                          (fun w callback ->
                            execute_sql env sql params
                            |> Promise.map (fun xs ->
                                   let (w : Io.world) =
                                     {
                                       log =
                                         ( query_params_to_yojson p,
                                           query_result_to_yojson xs )
                                         :: w.log;
                                     }
                                   in
                                   with_effect env (callback w) xs)
                            |> ignore);
                      })
            | Fetch ((url, props) as p) ->
                Some
                  (fun (k : (a, _) continuation) ->
                    continue k
                      {
                        f =
                          (fun w callback ->
                            let url = fix_url env url in
                            Js.Unsafe.fun_call Js.Unsafe.global##.fetch
                              [|
                                Js.Unsafe.inject url;
                                props |> Yojson.Safe.to_string |> Js.string
                                |> Json.unsafe_input;
                              |]
                            |> Promise.then_ ~fulfilled:(fun x -> x##text)
                            |> Promise.then_ ~fulfilled:(fun x ->
                                   let x = `Ok x in
                                   let (w : Io.world) =
                                     {
                                       log =
                                         ( fetch_params_to_yojson p,
                                           fetch_result_to_yojson x )
                                         :: w.log;
                                     }
                                   in
                                   with_effect env (callback w) x;
                                   Promise.return ())
                            |> ignore);
                      })
            | _ -> None);
      }
end

let handle_fetch request env =
  let open Promise.Syntax in
  let* (text : string) = request##text in
  let effect : unit Io.t Io.t =
    RealEffectHandlers.with_effect env handle_message text
  in
  Promise.make (fun ~resolve ~reject:_ ->
      effect.f { log = [] } (fun w e2 ->
          e2.f w (fun w _ ->
              `List
                (w.log |> List.rev |> List.map (fun (a, b) -> `List [ a; b ]))
              |> Yojson.Safe.pretty_to_string |> print_endline;
              resolve ())))

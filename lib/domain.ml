module Io = struct
  type 'a t = { f : ('a -> unit) -> unit }

  let map (f : 'a -> 'b) (ma : 'a t) : 'b t =
    { f = (fun callback -> ma.f (fun x -> callback (f x))) }

  let pure x = { f = (fun callback -> callback x) }

  let combine2 ma mb =
    { f = (fun callback -> ma.f (fun a -> mb.f (fun b -> callback (a, b)))) }

  let ignore ma = { f = (fun callback -> ma.f (fun _ -> callback ())) }

  module Syntax = struct
    let ( let+ ) ma f = map f ma

    module Monad = struct
      let ( let* ) (ma : 'a t) (f : 'a -> 'b t) : 'b t =
        {
          f =
            (fun callback ->
              ma.f (fun a ->
                  let mb = f a in
                  mb.f callback));
        }
    end
  end
end

type _ Effect.t +=
  | Fetch : (string * Yojson.Safe.t) -> Yojson.Safe.t Io.t Effect.t
  | QueryDbFx : (string * string list) -> Yojson.Safe.t list Io.t Effect.t
  | ExecuteDbFx : (string * string list) -> unit Io.t Effect.t

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
                 |> Yojson.Safe.pretty_to_string) );
             ("method", `String "POST");
             ("headers", `Assoc [ ("Content-Type", `String "application/json") ]);
           ] ))

let handle_ls_command user_id =
  let open Io.Syntax in
  let+ subs =
    Effect.perform
      (QueryDbFx
         ("SELECT * FROM new_subscriptions WHERE user_id = ?", [ user_id ]))
  in
  `List subs |> Yojson.Safe.pretty_to_string |> print_endline;
  let response_msg =
    match subs with
    | [] -> "No subscriptions"
    | subs ->
        subs
        |> List.map (fun x ->
               Yojson.Safe.Util.member "content" x
               |> Yojson.Safe.Util.to_string |> Yojson.Safe.from_string
               |> Yojson.Safe.Util.member "url"
               |> Yojson.Safe.Util.to_string)
        |> List.fold_left (Printf.sprintf "%s\n%s") "Subscriptions:"
  in
  send_telegram_mesage user_id response_msg |> Io.ignore

let handle_add_command user_id url =
  let open Io.Syntax in
  let response_msg = "Subscription added" in
  let content = `Assoc [ ("url", `String url) ] |> Yojson.Safe.to_string in
  let f1 =
    Effect.perform
      (ExecuteDbFx
         ( "INSERT INTO new_subscriptions (user_id, content) VALUES (?, ?)",
           [ user_id; content ] ))
  in
  let f2 = send_telegram_mesage user_id response_msg in
  let+ _ = Io.pure () in
  Io.combine2 f1 f2 |> Io.ignore

let handle_message message =
  let module S = Spectator in
  let msg_model =
    message |> Yojson.Safe.from_string |> S.message_upd_of_yojson
  in
  match msg_model with
  | Ok msg_model -> (
      let user_id = string_of_int msg_model.message.from.id in
      match String.split_on_char ' ' msg_model.message.text with
      | [ "/ls" ] -> handle_ls_command user_id
      | [ "/add"; url ] -> handle_add_command user_id url
      | _ -> failwith __LOC__)
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

  let log_query tag sql params =
    print_endline @@ tag ^ "\n"
    ^ (`Assoc
         [
           ("sql", `String sql);
           ("params", `List (params |> List.map (fun x -> `String x)));
         ]
      |> Yojson.Safe.pretty_to_string)

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
            | QueryDbFx (sql, params) ->
                Some
                  (fun (k : (a, _) continuation) ->
                    continue k
                      {
                        f =
                          (fun callback ->
                            log_query "[QueryDbFx]" sql params;
                            execute_sql env sql params
                            |> Promise.map (fun xs ->
                                   with_effect env callback xs)
                            |> ignore);
                      })
            | ExecuteDbFx (sql, params) ->
                (* print_endline @@ "[ExecuteDbFx]"; *)
                Some
                  (fun (k : (a, _) continuation) ->
                    continue k
                      {
                        f =
                          (fun callback ->
                            log_query "[ExecuteDbFx]" sql params;
                            execute_sql env sql params
                            |> Promise.map (fun _ ->
                                   with_effect env callback ())
                            |> ignore);
                      })
            | Fetch (url, props) ->
                (* print_endline @@ "[Fetch] " ^ url ^ "\n"
                   ^ Yojson.Safe.pretty_to_string props; *)
                Some
                  (fun (k : (a, _) continuation) ->
                    continue k
                      {
                        f =
                          (fun callback ->
                            let url = fix_url env url in
                            (* Js.Unsafe.meth_call
                                 (Js.Unsafe.pure_js_expr "console")
                                 "info"
                                 [|
                                   Js.Unsafe.inject url;
                                   props |> Yojson.Safe.to_string |> Js.string
                                   |> Json.unsafe_input;
                                 |]
                               |> ignore; *)
                            Js.Unsafe.fun_call Js.Unsafe.global##.fetch
                              [|
                                Js.Unsafe.inject url;
                                props |> Yojson.Safe.to_string |> Js.string
                                |> Json.unsafe_input;
                              |]
                            |> Promise.then_ ~fulfilled:(fun x -> x##text)
                            |> Promise.then_ ~fulfilled:(fun x ->
                                   x |> Yojson.Safe.from_string
                                   |> Promise.return)
                            |> Promise.then_ ~fulfilled:(fun x ->
                                   with_effect env callback x;
                                   Promise.return ())
                            |> ignore);
                      })
            | _ -> None);
      }
end

let handle_fetch request env =
  let open Promise.Syntax in
  let* (text : string) = request##text in
  (* print_endline text; *)
  let effect : unit Io.t Io.t =
    RealEffectHandlers.with_effect env handle_message text
  in
  Promise.make (fun ~resolve ~reject:_ ->
      effect.f (fun e2 -> e2.f (fun _ -> resolve ())))

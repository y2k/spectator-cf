open Effect_handlers

(* open DbQuery *)

module Handler = struct
  module StringMap = Map.Make (String)

  type state = { subs : string list StringMap.t }

  let empty_state = { subs = StringMap.empty }

  let handle_command' (state : state) (user_id : string) (msg : string) =
    match String.split_on_char ' ' msg with
    | [ "/ls" ] ->
        let subs =
          state.subs |> StringMap.find_opt user_id |> Option.value ~default:[]
        in
        ( state,
          if subs = [] then "No subscriptions"
          else
            subs |> List.fold_left (Printf.sprintf "%s\n- %s") "Subscriptions:"
        )
    | [ "/add"; url ] ->
        let subs =
          state.subs |> StringMap.find_opt user_id |> Option.value ~default:[]
        in
        let subs = url :: subs in
        let state = { subs = StringMap.add user_id subs state.subs } in
        (state, "Subscription added")
    | _ -> failwith "???"

  let handle_command () =
    let state = ref empty_state in
    fun (user_id : string) (msg : string) ->
      let s, result = handle_command' !state user_id msg in
      state := s;
      result
end

type message_from = { id : int } [@@deriving yojson { strict = false }]

type message = { text : string; from : message_from }
[@@deriving yojson { strict = false }]

type message_upd = { message : message } [@@deriving yojson { strict = false }]

let send_to_user user_id message =
  let url = "https://api.telegram.org/bot~TG_TOKEN~/sendMessage" in
  let props =
    `Assoc
      [
        ( "body",
          `String
            (`Assoc [ ("chat_id", `String user_id); ("text", `String message) ]
            |> Yojson.pretty_to_string) );
        ("method", `String "POST");
        ("headers", `Assoc [ ("Content-Type", `String "application/json") ]);
      ]
  in
  Effect.perform (Fetch (url, props))

let statefull_handler = Handler.handle_command ()

let handle_command text =
  let json = text |> Yojson.Safe.from_string in
  json |> message_upd_of_yojson
  |> Result.map (fun x ->
         statefull_handler (string_of_int x.message.from.id) x.message.text
         |> send_to_user (string_of_int x.message.from.id))
  |> Result.fold ~ok:Fun.id ~error:(fun _e ->
         failwith "Unknown telegram command not supported")

let _handle_fetch request =
  let open Promise.Syntax in
  let* text = request##text in
  let msg_promise = decorate_with_fetch_effect handle_command text in
  let+ a = msg_promise () in
  match a with Ok _x -> () | Error _e -> ()

module DbQuery = struct
  type new_subscription = { user_id : string; url : string } [@@deriving yojson]
  type _ Effect.t += Sql : Yojson.Safe.t -> Yojson.Safe.t Thunk.t Effect.t

  let rec to_sql = function
    | `String x -> x
    | `List [ `String x; a; b ] ->
        to_sql a ^ " " ^ String.uppercase_ascii x ^ " " ^ to_sql b
    | `Assoc _xs -> (
        _xs
        |> List.map (fun (k, v) ->
               Printf.sprintf "%s %s" (String.uppercase_ascii k) (to_sql v))
        |> function
        | x :: xs -> List.fold_left (Printf.sprintf "%s %s") x xs
        | xs -> List.fold_left (Printf.sprintf "%s %s") "" xs)
    | x -> failwith @@ "Node not supported: " ^ Yojson.Safe.pretty_to_string x
end

let handle_fetch _request env =
  let open Js_of_ocaml in
  (* let sql =
     `Assoc
       [
         ("select", `String "*");
         ("from", `String "new_subscriptions");
         ("where", `List [ `String "="; `String "user_id"; `String "?" ]);
       ]
     |> trace_ex "JSQL" Yojson.Safe.pretty_to_string
     |> DbQuery.to_sql |> trace "SQL" *)
  (* let sql = "INSERT INTO new_subscriptions (user_id, content) VALUES (?, ?)" in
     let open Promise.Syntax in
     let* result = ((env ##. DB##prepare sql)##bind "u1" "c2")##run in
     trace "Result" result |> ignore; *)
  (*  *)
  let _jsql =
    `Assoc
      [
        ("query", `String "SELECT * FROM new_subscriptions WHERE user_id = ?");
        ("type", `String "raw");
        ("params", `Assoc []);
      ]
  in
  let sql = "SELECT * FROM new_subscriptions WHERE user_id = ?" in
  let open Promise.Syntax in
  let* result = ((env ##. DB##prepare sql)##bind "u1")##run in
  trace "Result"
    (result##.results |> Json.output |> Js.to_string |> Yojson.Safe.from_string
   |> Yojson.Safe.pretty_to_string)
  |> ignore;
  Promise.return ()

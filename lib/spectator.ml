open Effect_handlers

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

let handle_fetch request =
  let open Promise.Syntax in
  let* text = request##text in
  let msg_promise = decorate_with_fetch_effect handle_command text in
  let+ a = msg_promise () in
  match a with Ok _x -> () | Error _e -> ()

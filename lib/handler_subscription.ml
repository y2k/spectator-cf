open Effects

let rss_type = "b18036ba-0838-437a-8925-7524cf8b07b9"
let atom_re = Re.Perl.compile_pat {|<feed xmlns="http://www.w3.org/2005/Atom"|}
let is_atom content = Re.execp atom_re content

let get_ids new_subs =
  new_subs
  |> List.map (fun x ->
         let module U = Yojson.Safe.Util in
         let id = x |> U.member "id" |> U.to_int in
         let user_id = x |> U.member "user_id" |> U.to_string in
         let url =
           x |> U.member "content" |> U.to_string |> Yojson.Safe.from_string
           |> U.member "url" |> U.to_string
         in
         (id, user_id, url))

(* ================================== *)

let get_new_subs = Db.query ("SELECT * FROM new_subscriptions LIMIT 5", [])

let get_new_sub_contents new_subs =
  get_ids new_subs
  |> List.map (fun (_, _, url) -> Db.fetch (url, `Assoc []))
  |> Io.combine

let save_subs new_subs contents =
  (* print_endline @@ "[LOG] save_subs.new_subs: "
     ^ (`List new_subs |> Yojson.Safe.pretty_to_string);
     print_endline @@ "[LOG] save_subs.contents: "
     ^ (`List (List.map fetch_result_to_yojson contents)
       |> Yojson.Safe.pretty_to_string); *)
  let rss_list =
    List.map2
      (fun (id, user_id, url) result ->
        match result with
        | Ok content when is_atom content -> Some (id, user_id, url)
        | _ -> None)
      (get_ids new_subs) contents
    |> List.filter_map Fun.id
  in
  (rss_list
  |> List.map (fun (id, _, _) ->
         Db.query
           ("DELETE FROM new_subscriptions WHERE id = ?", [ string_of_int id ]))
  )
  @ (rss_list
    |> List.map (fun (_, user_id, url) ->
           let content =
             `Assoc [ ("type", `String rss_type); ("url", `String url) ]
             |> Yojson.Safe.to_string
           in
           Db.query
             ( "INSERT INTO subscriptions (user_id, content) VALUES (?, ?)",
               [ user_id; content ] )))
  |> Io.combine

(* ================================== *)

let handle_ =
  let open Io.Syntax in
  let* new_subs = get_new_subs in
  let* rss_list = get_new_sub_contents new_subs in
  let* _ = save_subs new_subs rss_list in
  Io.pure ()

let handle env =
  let effect : unit Io.t = handle_ in
  Promise.make (fun ~resolve ~reject:_ ->
      let w : Io.world =
        { perform = Io.unhandled }
        |> RealEffectHandlers.attach_db_effect env
        |> RealEffectHandlers.attach_fetch_effect env
        |> RealEffectHandlers.attach_log_effect
      in
      effect.f w (fun _ () -> resolve ()))

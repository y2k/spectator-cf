open Effects

let atom_re = Re.Perl.compile_pat {|<feed xmlns="http://www.w3.org/2005/Atom">|}
let is_atom content = Re.execp atom_re content

let handle_ =
  let open Io.Syntax in
  let open Io.Syntax.Monad in
  let* new_subs =
    Effect.perform (QueryDbFx ("SELECT * FROM new_subscriptions LIMIT 5", []))
  in
  let id_urls =
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
  in
  let+ contents =
    id_urls
    |> List.map (fun (_, _, url) -> Effect.perform (Fetch (url, `Assoc [])))
    |> Io.combine
  in
  let rss_list =
    List.map2
      (fun (id, user_id, url) result ->
        match result with
        | Ok content when is_atom content -> Some (id, user_id, url)
        | _ -> None)
      id_urls contents
    |> List.filter_map Fun.id
  in
  (rss_list
  |> List.map (fun (id, _, _) ->
         Effect.perform
           (ExecuteDbFx
              ( "DELETE FROM new_subscriptions WHERE id = ?",
                [ string_of_int id ] ))))
  @ (rss_list
    |> List.map (fun (_, user_id, url) ->
           let content =
             `Assoc [ ("user_id", `String user_id); ("url", `String url) ]
             |> Yojson.Safe.to_string
           in
           Effect.perform
             (ExecuteDbFx
                ( "INSERT INTO subscriptions (user_id, content) VALUES (?, ?)",
                  [ user_id; content ] ))))
  |> Io.combine |> Io.ignore

let handle env =
  let effect : unit Io.t Io.t =
    Domain.RealEffectHandlers.with_effect env (fun _ -> handle_) ()
  in
  Promise.make (fun ~resolve ~reject:_ ->
      effect.f (fun e2 -> e2.f (fun _ -> resolve ())))

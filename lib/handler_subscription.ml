open Effects

let rss_type = "b18036ba-0838-437a-8925-7524cf8b07b9"
let atom_re = Re.Perl.compile_pat {|<feed xmlns="http://www.w3.org/2005/Atom"|}
let is_atom content = Re.execp atom_re content

let get_ids new_subs =
  new_subs
  |> List.map (fun row ->
         let module U = Yojson.Safe.Util in
         let id = row |> U.member "id" |> U.to_int in
         let content =
           row |> U.member "content" |> U.to_string |> Yojson.Safe.from_string
         in
         let url = content |> U.member "url" |> U.to_string in
         let user_id = content |> U.member "user_id" |> U.to_string in
         (id, user_id, url))

let get_new_subs = Effects.query "SELECT * FROM new_subscriptions LIMIT 5" []

let get_new_sub_contents new_subs =
  get_ids new_subs
  |> List.map (fun (_, _, url) -> Effects.fetch url (`Assoc []))
  |> Io.combine

let save_subs new_subs contents =
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
         Effects.query "DELETE FROM new_subscriptions WHERE id = ?"
           [ string_of_int id ]))
  @ (rss_list
    |> List.map (fun (_, user_id, url) ->
           let content =
             `Assoc
               [
                 ("type", `String rss_type);
                 ("url", `String url);
                 ("user_id", `String user_id);
               ]
             |> Yojson.Safe.to_string
           in
           Effects.query "INSERT INTO subscriptions (content) VALUES (?)"
             [ content ]))
  |> Io.combine

let handle =
  let open Io.Syntax in
  let* new_subs = get_new_subs in
  let* rss_list = get_new_sub_contents new_subs in
  save_subs new_subs rss_list |> Io.ignore

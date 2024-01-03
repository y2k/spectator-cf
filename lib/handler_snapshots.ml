type snapshot = { id : string }

type subscription = {
  user_id : string;
  url : string;
  updated : (float option[@default None]);
  last_id : (string option[@default None]);
}
[@@deriving yojson]

let get_new_snapshot last_id content =
  let snapshot =
    Xml.parse_string content
    |> Xml.map (fun x -> match Xml.tag x with "entry" -> Some x | _ -> None)
    |> List.filter_map Fun.id
    |> List.map (fun x ->
           let get_prop name =
             Xml.children x
             |> List.find (fun x -> Xml.tag x = name)
             |> Xml.children |> List.hd |> Xml.pcdata
           in
           { id = get_prop "id" })
    |> Fun.flip List.nth_opt 0
  in
  match snapshot with Some x when Some x.id <> last_id -> Some x | _ -> None

let handle =
  let open Effects in
  let open Io.Syntax in
  let* _subs =
    Effects.query
      "SELECT * FROM subscriptions LIMIT 5 ORDER BY content->>'updated'" []
  in
  let subs =
    _subs
    |> List.map (fun x ->
           let module U = Yojson.Safe.Util in
           x |> U.member "content" |> U.to_string |> Yojson.Safe.from_string
           |> subscription_of_yojson
           |> Result.fold ~ok:Fun.id ~error:(fun e ->
                  failwith @@ e ^ ":" ^ __LOC__))
  in
  let* _download_results =
    subs
    |> List.map (fun sub -> Effects.fetch sub.url (`Assoc []))
    |> Io.combine
  in
  _download_results
  |> List.map2
       (fun sub r ->
         match r with
         | Ok r -> (
             let _a = get_new_snapshot sub.last_id r in
             let _e =
               Effects.query "UPDATE subscriptions SET content = ? WHERE id = ?"
                 []
             in
             match _a with Some _a -> failwith "???" | None -> Io.pure ())
         | Error _ -> Io.pure ())
       subs
  |> Io.combine |> Io.ignore

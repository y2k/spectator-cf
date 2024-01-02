open Effects

let _handle =
  let open Io.Syntax in
  let* subs =
    Effects.query
      "SELECT * FROM subscriptions LIMIT 5 ORDER BY content->>'updated'" []
  in
  let module U = Yojson.Safe.Util in
  let* _download_results =
    subs
    |> List.map (fun x ->
           x |> U.member "content" |> U.to_string |> Yojson.Safe.from_string
           |> U.member "url" |> U.to_string
           |> Fun.flip Effects.fetch (`Assoc []))
    |> Io.combine
  in
  Io.pure ()

open Effects

let handle_ =
  let open Io.Syntax in
  let+ new_subs =
    Effect.perform (QueryDbFx ("SELECT * FROM new_subscriptions LIMIT 5", []))
  in
  let+ _urls =
    new_subs
    |> List.map (fun x ->
           let module U = Yojson.Safe.Util in
           let url =
             x |> U.member "content" |> U.to_string |> Yojson.Safe.from_string
             |> U.member "url" |> U.to_string
           in
           Effect.perform (Fetch (url, `Assoc [])))
    |> Io.combine
  in
  ()

let handle env =
  let effect : unit Io.t Io.t =
    Domain.RealEffectHandlers.with_effect env (fun _ -> handle_) ()
  in
  Promise.make (fun ~resolve ~reject:_ ->
      effect.f (fun e2 -> e2.f (fun _ -> resolve ())))

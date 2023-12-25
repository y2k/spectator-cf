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
  end

  let rec combine mas =
    let open Syntax in
    match mas with
    | mx :: mxs ->
        let+ x, xs = combine2 mx (combine mxs) in
        x :: xs
    | [] -> pure []
end

type _ Effect.t +=
  | Fetch : (string * Yojson.Safe.t) -> (string, string) Result.t Io.t Effect.t
  | QueryDbFx : (string * string list) -> Yojson.Safe.t list Io.t Effect.t
  | ExecuteDbFx : (string * string list) -> unit Io.t Effect.t

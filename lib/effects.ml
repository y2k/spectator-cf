module Io = struct
  type world = { log : (Yojson.Safe.t * Yojson.Safe.t) list }
  type 'a t = { f : world -> (world -> 'a -> unit) -> unit }

  let map (f : 'a -> 'b) (ma : 'a t) : 'b t =
    { f = (fun w callback -> ma.f w (fun w2 x -> callback w2 (f x))) }

  let pure x = { f = (fun w callback -> callback w x) }
  let never = { f = (fun _ _ -> ()) }

  module Syntax = struct
    let ( let+ ) ma f = map f ma

    module Monad = struct
      let ( let* ) ma f =
        {
          f =
            (fun w callback ->
              ma.f w (fun w2 a ->
                  let mb = f a in
                  mb.f w2 callback));
        }
    end
  end

  let combine2 ma mb =
    let open Syntax in
    let open Syntax.Monad in
    let* a = ma in
    let+ b = mb in
    (a, b)

  let ignore ma =
    { f = (fun w callback -> ma.f w (fun w2 _ -> callback w2 ())) }

  let rec combine mas =
    let open Syntax in
    match mas with
    | mx :: mxs ->
        let+ x, xs = combine2 mx (combine mxs) in
        x :: xs
    | [] -> pure []
end

type query_params = string * string list [@@deriving yojson]
type query_result = Yojson.Safe.t list [@@deriving yojson]
type fetch_params = string * Yojson.Safe.t [@@deriving yojson]
type fetch_result = [ `Ok of string | `Error of string ] [@@deriving yojson]

type _ Effect.t +=
  | Fetch : fetch_params -> fetch_result Io.t Effect.t
  | QueryDbFx : query_params -> query_result Io.t Effect.t

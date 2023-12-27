module Io = struct
  type 'a t = { f : ('a -> unit) -> unit }

  let map (f : 'a -> 'b) (ma : 'a t) : 'b t =
    { f = (fun callback -> ma.f (fun x -> callback (f x))) }

  let pure x = { f = (fun callback -> callback x) }
  let never = { f = ignore }

  let combine2 ma mb =
    { f = (fun callback -> ma.f (fun a -> mb.f (fun b -> callback (a, b)))) }

  let ignore ma = { f = (fun callback -> ma.f (fun _ -> callback ())) }

  module Syntax = struct
    let ( let+ ) ma f = map f ma

    module Monad = struct
      let ( let* ) ma f =
        {
          f =
            (fun callback ->
              ma.f (fun a ->
                  let mb = f a in
                  mb.f callback));
        }
    end
  end

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

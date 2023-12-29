module Io = struct
  type world = { log : Yojson.Safe.t list }
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
type fetch_result = (string, string) result

let fetch_result_to_yojson (x : fetch_result) : Yojson.Safe.t =
  (match x with
  | Ok x -> `Ok (x |> Digest.string |> Digest.to_hex)
  | Error e -> `Error e)
  |> [%to_yojson: [ `Ok of string | `Error of string ]]

type _ Effect.t +=
  | Fetch : fetch_params -> fetch_result Io.t Effect.t
  | QueryDbFx : query_params -> query_result Io.t Effect.t

module RealEffectHandlers = struct
  open Effect.Deep
  open Js_of_ocaml

  let execute_sql env (sql : string) (params : string list) :
      Yojson.Safe.t list Promise.t =
    let open Promise.Syntax in
    let+ result =
      (Js.Unsafe.meth_call
         (env ##. DB##prepare sql)
         "bind"
         (params |> List.map Js.Unsafe.inject |> Array.of_list))##run
    in
    result##.results |> Json.output |> Js.to_string |> Yojson.Safe.from_string
    |> function
    | `List xs -> xs
    | x -> [ x ]

  let fix_url env url =
    let token = env ##. TG_TOKEN_ in
    url |> String.split_on_char '~'
    |> List.map (function "TG_TOKEN" -> token | x -> x)
    |> List.fold_left ( ^ ) ""

  let rec with_effect : 'a 'b. _ -> ('a -> 'b) -> 'a -> 'b =
   fun env f x ->
    try_with f x
      {
        effc =
          (fun (type a) (eff : a Effect.t) ->
            match eff with
            | QueryDbFx ((sql, params) as p) ->
                Some
                  (fun (k : (a, _) continuation) ->
                    continue k
                      {
                        f =
                          (fun w callback ->
                            execute_sql env sql params
                            |> Promise.map (fun xs ->
                                   let (w : Io.world) =
                                     {
                                       log =
                                         `Assoc
                                           [
                                             ("name", `String "query");
                                             ("in", query_params_to_yojson p);
                                             ("out", query_result_to_yojson xs);
                                           ]
                                         :: w.log;
                                     }
                                   in
                                   with_effect env (callback w) xs)
                            |> ignore);
                      })
            | Fetch ((url, props) as p) ->
                Some
                  (fun (k : (a, _) continuation) ->
                    continue k
                      {
                        f =
                          (fun w callback ->
                            let url = fix_url env url in
                            Js.Unsafe.fun_call Js.Unsafe.global##.fetch
                              [|
                                Js.Unsafe.inject url;
                                props |> Yojson.Safe.to_string |> Js.string
                                |> Json.unsafe_input;
                              |]
                            |> Promise.then_ ~fulfilled:(fun x -> x##text)
                            |> Promise.then_ ~fulfilled:(fun x ->
                                   let x = Ok x in
                                   let (w : Io.world) =
                                     {
                                       log =
                                         `Assoc
                                           [
                                             ("name", `String "fetch");
                                             ("in", fetch_params_to_yojson p);
                                             ("out", fetch_result_to_yojson x);
                                           ]
                                         :: w.log;
                                     }
                                   in
                                   with_effect env (callback w) x;
                                   Promise.return ())
                            |> ignore);
                      })
            | _ -> None);
      }
end

module Io = struct
  type world = { perform : Yojson.Safe.t -> Yojson.Safe.t t }
  and 'a t = { f : world -> (world -> 'a -> unit) -> unit }

  let unhandled (p : Yojson.Safe.t) : Yojson.Safe.t t =
    {
      f =
        (fun _ _ -> failwith @@ "Effect unhandled: " ^ Yojson.Safe.to_string p);
    }

  let map (f : 'a -> 'b) (ma : 'a t) : 'b t =
    { f = (fun w callback -> ma.f w (fun w2 x -> callback w2 (f x))) }

  let pure x = { f = (fun w callback -> callback w x) }
  let never = { f = (fun _ _ -> ()) }
  let resolve_world : world t = { f = (fun w callback -> callback w w) }

  module Syntax = struct
    let ( let+ ) ma f = map f ma

    let ( let* ) ma f =
      {
        f =
          (fun w callback ->
            ma.f w (fun w2 a ->
                let mb = f a in
                mb.f w2 callback));
      }
  end

  let combine2 ma mb =
    let open Syntax in
    let* a = ma in
    let* b = mb in
    pure (a, b)

  let ignore ma =
    { f = (fun w callback -> ma.f w (fun w2 _ -> callback w2 ())) }

  let rec combine mas =
    let open Syntax in
    match mas with
    | mx :: mxs ->
        let* x = mx in
        let* xs = combine mxs in
        pure (x :: xs)
    | [] -> pure []
end

type query_params = string * string list [@@deriving yojson]
type query_result = Yojson.Safe.t list [@@deriving yojson]
type fetch_params = string * Yojson.Safe.t [@@deriving yojson]
type fetch_result = (string, string) result

let read_const : string Io.t =
  let open Io.Syntax in
  let* w = Io.resolve_world in
  let+ result = w.perform (`Assoc [ ("name", `String "const") ]) in
  Yojson.Safe.Util.to_string result

let fetch_result_to_yojson (x : fetch_result) : Yojson.Safe.t =
  (match x with Ok x -> `Ok x | Error e -> `Error e)
  |> [%to_yojson: [ `Ok of string | `Error of string ]]

let fetch_result_of_yojson (json : Yojson.Safe.t) =
  json |> [%of_yojson: [ `Ok of string | `Error of string ]]
  |> Result.map (fun x -> match x with `Ok x -> Ok x | `Error x -> Error x)

let query (params : query_params) : query_result Io.t =
  let open Io.Syntax in
  let* w = Io.resolve_world in
  let+ result =
    `Assoc
      [ ("name", `String "database"); ("in", query_params_to_yojson params) ]
    |> w.perform
  in
  result |> query_result_of_yojson |> Result.get_ok

let fetch (params : fetch_params) : fetch_result Io.t =
  let open Io.Syntax in
  let* w = Io.resolve_world in
  let+ result =
    `Assoc [ ("name", `String "fetch"); ("in", fetch_params_to_yojson params) ]
    |> w.perform
  in
  result |> fetch_result_of_yojson |> Result.get_ok

module Impl = struct
  open Js_of_ocaml

  let attach_const_effect (const : string) (w : Io.world) =
    {
      Io.perform =
        (fun p ->
          let module U = Yojson.Safe.Util in
          match p |> U.member "name" with
          | `String "const" -> Io.pure (`String const)
          | _ -> w.perform p);
    }

  let attach_log_effect (w : Io.world) : Io.world =
    let rec pretty_to_string_ex = function
      | `String x when String.length x > 512 ->
          `String ("md5:" ^ (Digest.string x |> Digest.to_hex))
      | `List xs -> `List (List.map pretty_to_string_ex xs)
      | `Assoc xs ->
          `Assoc (List.map (fun (k, v) -> (k, pretty_to_string_ex v)) xs)
      | x -> x
    in
    {
      perform =
        (fun p ->
          let open Io.Syntax in
          let+ result = w.perform p in
          let log_p = p |> Yojson.Safe.Util.to_assoc in
          let log_result = pretty_to_string_ex result in
          `Assoc (log_p @ [ ("out", log_result) ])
          |> Yojson.Safe.pretty_to_string |> print_endline;
          result);
    }

  let attach_db_effect env (w : Io.world) : Io.world =
    let perform (p : Yojson.Safe.t) : Yojson.Safe.t Io.t =
      let module U = Yojson.Safe.Util in
      match p |> U.member "name" |> U.to_string with
      | "database" ->
          let execute_sql (sql : string) (params : string list) :
              Yojson.Safe.t list Promise.t =
            let open Promise.Syntax in
            let+ result =
              (Js.Unsafe.meth_call
                 (env ##. DB##prepare sql)
                 "bind"
                 (params |> List.map Js.Unsafe.inject |> Array.of_list))##run
            in
            result##.results |> Json.output |> Js.to_string
            |> Yojson.Safe.from_string
            |> function
            | `List xs -> xs
            | x -> [ x ]
          in
          let q, ps =
            p |> U.member "in" |> query_params_of_yojson |> Result.get_ok
          in
          {
            f =
              (fun w2 callback ->
                execute_sql q ps
                |> Promise.then_
                     ~fulfilled:(fun x ->
                       callback w2 (`List x);
                       Promise.return ())
                     ~rejected:(fun _x ->
                       prerr_endline @@ "[DB] ERROR " ^ __LOC__;
                       Promise.return ())
                |> ignore);
          }
      | _ -> w.perform p
    in
    { perform }

  let attach_fetch_effect env (w0 : Io.world) : Io.world =
    let perform (p : Yojson.Safe.t) : Yojson.Safe.t Io.t =
      let module U = Yojson.Safe.Util in
      match p |> U.member "name" |> U.to_string with
      | "fetch" ->
          let fix_url url =
            let token = env ##. TG_TOKEN_ in
            url |> String.split_on_char '~'
            |> List.map (function "TG_TOKEN" -> token | x -> x)
            |> List.fold_left ( ^ ) ""
          in
          let url, props =
            p |> U.member "in" |> fetch_params_of_yojson |> Result.get_ok
          in
          let url = fix_url url in
          {
            f =
              (fun w2 callback ->
                Js.Unsafe.fun_call Js.Unsafe.global##.fetch
                  [|
                    Js.Unsafe.inject url;
                    props |> Yojson.Safe.to_string |> Js.string
                    |> Json.unsafe_input;
                  |]
                |> Promise.then_ ~fulfilled:(fun x -> x##text)
                |> Promise.then_ ~fulfilled:(fun x ->
                       callback w2 (fetch_result_to_yojson (Ok x));
                       Promise.return ())
                |> ignore);
          }
      | _ -> w0.perform p
    in
    { perform }
end

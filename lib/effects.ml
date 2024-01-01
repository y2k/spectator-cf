module Io = struct
  type world = {
    perform : world -> Yojson.Safe.t -> (world -> Yojson.Safe.t -> unit) -> unit;
  }

  let unhandled (_ : world) p _ : unit =
    failwith @@ "Effect unhandled: " ^ Yojson.Safe.to_string p

  type 'a t = { f : world -> (world -> 'a -> unit) -> unit }

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

let fetch_result_to_yojson (x : fetch_result) : Yojson.Safe.t =
  (match x with
  (* | Ok x -> `Ok (x |> Digest.string |> Digest.to_hex) *)
  | Ok x -> `Ok x
  | Error e -> `Error e)
  |> [%to_yojson: [ `Ok of string | `Error of string ]]

let fetch_result_of_yojson (json : Yojson.Safe.t) =
  json |> [%of_yojson: [ `Ok of string | `Error of string ]]
  |> Result.map (fun x -> match x with `Ok x -> Ok x | `Error x -> Error x)

module Db = struct
  let query (params : query_params) : query_result Io.t =
    {
      f =
        (fun w c ->
          let ap =
            `Assoc
              [
                ("name", `String "database");
                ("in", query_params_to_yojson params);
              ]
          in
          w.perform w ap (fun w x ->
              match x with `List xs -> c w xs | x -> c w [ x ]));
    }

  let fetch (params : fetch_params) : fetch_result Io.t =
    {
      f =
        (fun w c ->
          let ap =
            `Assoc
              [
                ("name", `String "fetch"); ("in", fetch_params_to_yojson params);
              ]
          in
          w.perform w ap (fun w x ->
              c w (fetch_result_of_yojson x |> Result.get_ok)));
    }
end

type _ Effect.t +=
  | Fetch : fetch_params -> fetch_result Io.t Effect.t
  | QueryDbFx : query_params -> query_result Io.t Effect.t

module RealEffectHandlers = struct
  open Js_of_ocaml

  let format_json_for_log = function
    (* | `String x when String.starts_with ~prefix:"{" x ->
           `Assoc [ ("<JSON>", Yojson.Safe.from_string x) ]
       | `List xs -> `List (List.map format_json_for_log xs)
       | `Assoc xs ->
           `Assoc (List.map (fun (k, v) -> (k, format_json_for_log v)) xs) *)
    | x -> x

  let attach_log_effect (w : Io.world) : Io.world =
    let perform (w2 : Io.world) (p : Yojson.Safe.t)
        (callback : _ -> Yojson.Safe.t -> unit) =
      let module U = Yojson.Safe.Util in
      w.perform w2 p (fun w result ->
          let log_p = p |> format_json_for_log |> Yojson.Safe.Util.to_assoc in
          let log_result = format_json_for_log result in
          `Assoc (log_p @ [ ("out", log_result) ])
          |> Yojson.Safe.pretty_to_string |> print_endline;
          callback w result)
    in
    { perform }

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

  let attach_db_effect env (w : Io.world) : Io.world =
    let perform (w2 : Io.world) (p : Yojson.Safe.t)
        (callback : _ -> Yojson.Safe.t -> unit) =
      let module U = Yojson.Safe.Util in
      match p |> U.member "name" |> U.to_string with
      | "database" ->
          let q, ps =
            p |> U.member "in" |> query_params_of_yojson |> Result.get_ok
          in
          execute_sql env q ps
          |> Promise.then_ ~fulfilled:(fun x ->
                 callback w2 (`List x);
                 Promise.return ())
          |> ignore
      | _ -> w.perform w2 p callback
    in
    { perform }

  let fix_url env url =
    let token = env ##. TG_TOKEN_ in
    url |> String.split_on_char '~'
    |> List.map (function "TG_TOKEN" -> token | x -> x)
    |> List.fold_left ( ^ ) ""

  let attach_fetch_effect env (w0 : Io.world) : Io.world =
    let perform (w2 : Io.world) (p : Yojson.Safe.t)
        (callback : _ -> Yojson.Safe.t -> unit) =
      let module U = Yojson.Safe.Util in
      match p |> U.member "name" |> U.to_string with
      | "fetch" ->
          let url, props =
            p |> U.member "in" |> fetch_params_of_yojson |> Result.get_ok
          in

          let url = fix_url env url in
          Js.Unsafe.fun_call Js.Unsafe.global##.fetch
            [|
              Js.Unsafe.inject url;
              props |> Yojson.Safe.to_string |> Js.string |> Json.unsafe_input;
            |]
          |> Promise.then_ ~fulfilled:(fun x -> x##text)
          |> Promise.then_ ~fulfilled:(fun x ->
                 callback w2 (fetch_result_to_yojson (Ok x));
                 Promise.return ())
          |> ignore
      | _ -> w0.perform w2 p callback
    in
    { perform }
end

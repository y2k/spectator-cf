open Js_of_ocaml

let trace prefix x =
  Js.Unsafe.meth_call Js.Unsafe.global##.console "log"
    [| Js.Unsafe.inject (prefix ^ ":"); Js.Unsafe.inject x |]
  |> ignore;
  x

module Thunk = struct
  type 'a t = unit -> ('a, string) Result.t Promise.t
end

type _ Effect.t += Fetch : (string * Yojson.Safe.t) -> unit Thunk.t Effect.t

open Js_of_ocaml

let fix_url url =
  url |> String.split_on_char '~'
  |> List.map (function "TG_TOKEN" -> Js.Unsafe.global ##. TG_TOKEN_ | x -> x)
  |> List.fold_left ( ^ ) ""

let decorate_with_fetch_effect f arg =
  let open Effect.Deep in
  try_with f arg
    {
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Fetch (url, props) ->
              Some
                (fun (k : (a, _) continuation) ->
                  let thunk_action () =
                    Js.Unsafe.fun_call Js.Unsafe.global##.fetch
                      [|
                        Js.Unsafe.inject (fix_url url);
                        props |> Yojson.Safe.to_string |> Js.string
                        |> Json.unsafe_input;
                      |]
                    |> Promise.Result.from_catch
                    |> Promise.then_ ~fulfilled:(function
                         | Ok _ -> Ok () |> Promise.return
                         | Error _e -> Error "FIMXE" |> Promise.return)
                  in
                  continue k thunk_action)
          | _ -> None);
    }

open Lib.Effects

module Intergration_tests = struct
  let create_sample_path filename = "../../../test/samples/" ^ filename

  let read_sample filename =
    let f = open_in (create_sample_path filename) in
    Fun.protect
      ~finally:(fun _ -> close_in f)
      (fun _ -> really_input_string f (in_channel_length f))

  let write_sample filename content =
    let oc = open_out (create_sample_path filename) in
    output_string oc content;
    close_out oc

  let sample_exists filename = Sys.file_exists (create_sample_path filename)

  let assert__ sample_file_name actual =
    if sample_exists sample_file_name then (
      let expected = read_sample sample_file_name in
      if expected <> actual then (
        prerr_endline @@ "=== ACTUAL ===\n" ^ actual ^ "\n=== EXPECTED ===\n"
        ^ expected ^ "\n===";
        failwith @@ "actual <> expected | " ^ sample_file_name))
    else write_sample sample_file_name actual
end

module IoSample2 = struct
  open Effect.Deep
  open Js_of_ocaml
  open Lib.Domain

  let rec with_effect : 'a 'b. _ -> _ -> ('a -> 'b) -> 'a -> 'b =
   fun effects_log query_stage f x ->
    try_with f x
      {
        effc =
          (fun (type a) (eff : a Effect.t) ->
            match eff with
            | QueryDbFx query ->
                let query_id = Hashtbl.hash query |> string_of_int in
                let content =
                  Intergration_tests.read_sample
                    (query_id ^ "." ^ query_stage ^ ".query.json")
                  |> Yojson.Safe.from_string
                in
                let result = match content with `List xs -> xs | x -> [ x ] in
                Some
                  (fun (k : (a, _) continuation) ->
                    continue k
                      {
                        f =
                          (fun callback ->
                            Dom_html.setTimeout
                              (fun () ->
                                with_effect effects_log query_stage callback
                                  result)
                              10.0
                            |> ignore);
                      })
            | ExecuteDbFx (query, params) ->
                Some
                  (fun (k : (a, _) continuation) ->
                    continue k
                      {
                        f =
                          (fun callback ->
                            Dom_html.setTimeout
                              (fun () ->
                                effects_log :=
                                  `Assoc
                                    [
                                      ("sql", `String query);
                                      ( "params",
                                        `List
                                          (params
                                          |> List.map (fun x -> `String x)) );
                                    ]
                                  :: !effects_log;
                                with_effect effects_log query_stage callback ())
                              10.0
                            |> ignore);
                      })
            | Fetch (url, req) ->
                Some
                  (fun (k : (a, _) continuation) ->
                    continue k
                      {
                        f =
                          (fun callback ->
                            Dom_html.setTimeout
                              (fun () ->
                                effects_log :=
                                  `Assoc
                                    [ ("url", `String url); ("props", req) ]
                                  :: !effects_log;
                                with_effect effects_log query_stage callback
                                  (Ok ""))
                              10.0
                            |> ignore);
                      })
            | _ -> None);
      }

  let reduce f = function
    | x :: xs -> xs |> List.fold_left f x
    | _ -> failwith "List is empty"

  let get_actual_effects_log effects_log =
    (match !effects_log with [ x ] -> x | xs -> `List xs)
    |> Yojson.Safe.pretty_to_string

  let assert_ sample stage =
    let effects_log = ref [] in
    let msg = Intergration_tests.read_sample sample in
    let effect : unit Io.t Io.t =
      with_effect effects_log stage handle_message msg
    in
    effect.f (fun e2 ->
        e2.f (fun _ ->
            get_actual_effects_log effects_log
            |> Intergration_tests.assert__ ("expected." ^ sample)))

  let () = assert_ "sample1.json" "1"
  let () = assert_ "sample2.json" "1"
  let () = assert_ "sample3.json" "2"
  let () = assert_ "sample4.json" "1"
end

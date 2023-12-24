module Intergration_tests = struct
  open Lib.Effect_handlers

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

  let decorate_with_mock_effect effect_params f arg =
    let open Effect.Deep in
    try_with f arg
      {
        effc =
          (fun (type a) (eff : a Effect.t) ->
            match eff with
            | Fetch (url, props) ->
                Some
                  (fun (k : (a, _) continuation) ->
                    continue k (fun _ ->
                        let p =
                          `Assoc [ ("url", `String url); ("props", props) ]
                          |> Yojson.Safe.pretty_to_string
                        in
                        effect_params := p;
                        Promise.return (Ok ())))
            | _ -> None);
      }

  let assert__ sample_file_name actual =
    if sample_exists sample_file_name then (
      let expected = read_sample sample_file_name in
      if expected <> actual then failwith actual)
    else write_sample sample_file_name actual

  let assert_ input_name =
    let json = read_sample input_name in
    let actual = ref "" in
    (decorate_with_mock_effect actual Lib.Spectator.handle_command json) ()
    |> ignore;
    let expected_filename = "expected." ^ input_name in
    assert__ expected_filename !actual

  let _ignore () = assert_ "sample1.json"

  let _ignore () =
    assert_ "sample2.json";
    assert_ "sample3.json"
end

module IoSample2 = struct
  open Lib.Domain

  let effects_log : Yojson.Safe.t list ref = ref []

  let rec with_effect : 'a 'b. ('a -> 'b) -> 'a -> 'b =
    let open Effect.Deep in
    fun f x ->
      try_with f x
        {
          effc =
            (fun (type a) (eff : a Effect.t) ->
              let open Js_of_ocaml in
              match eff with
              | QueryDbFx query ->
                  let query_id = Hashtbl.hash query |> string_of_int in
                  let content =
                    Intergration_tests.read_sample (query_id ^ ".query.json")
                    |> Yojson.Safe.from_string
                  in
                  let result =
                    match content with `List xs -> xs | x -> [ x ]
                  in
                  Some
                    (fun (k : (a, _) continuation) ->
                      continue k
                        {
                          f =
                            (fun callback ->
                              Dom_html.setTimeout
                                (fun () -> with_effect callback result)
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
                                  with_effect callback ())
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
                                  with_effect callback `Null)
                                10.0
                              |> ignore);
                        })
              | _ -> None);
        }

  let reduce f = function
    | x :: xs -> xs |> List.fold_left f x
    | _ -> failwith "List is empty"

  let get_actual_effects_log () =
    (match !effects_log with [ x ] -> x | xs -> `List xs)
    |> Yojson.Safe.pretty_to_string

  let test2 () =
    effects_log := [];
    let msg = Intergration_tests.read_sample "sample2.json" in
    let a : unit Io.t Io.t = with_effect handle_message msg in
    a.f (fun a ->
        a.f (fun _ ->
            let actual = get_actual_effects_log () in
            get_actual_effects_log ()
            |> Intergration_tests.assert__ "expected.sample2.json";
            ()))

  let () =
    effects_log := [];
    let msg = Intergration_tests.read_sample "sample1.json" in
    let effect : unit Io.t Io.t = with_effect handle_message msg in
    effect.f (fun e2 ->
        e2.f (fun _ ->
            get_actual_effects_log ()
            |> Intergration_tests.assert__ "expected.sample1.json";
            test2 ()))
end

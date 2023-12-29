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

  module TextComparer : sig
    val compare_2_txt : string -> string -> string -> unit
  end = struct
    let save_string_to_temp_file string =
      let temp_file_path = Filename.temp_file "compare" "" in
      let oc = open_out temp_file_path in
      Fun.protect
        (fun _ ->
          output_string oc string;
          temp_file_path)
        ~finally:(fun _ -> close_out oc)

    let compare_2_txt log sample1 sample2 =
      let tmp1 = save_string_to_temp_file sample1 in
      let tmp2 = save_string_to_temp_file sample2 in
      let result = Sys.command @@ Printf.sprintf "diff -u %s %s" tmp1 tmp2 in
      if result <> 0 then failwith log
  end

  let assert_ sample_file_name actual =
    if sample_exists sample_file_name then (
      let expected = read_sample sample_file_name in
      if expected <> actual then
        TextComparer.compare_2_txt
          ("actual <> expected [" ^ sample_file_name ^ "]")
          expected actual)
    else write_sample sample_file_name actual
end

let () =
  let test name =
    let actual =
      Intergration_tests.read_sample name |> Lib.Handler_subscription.is_atom
    in
    if not actual then failwith @@ name ^ " not RSS/ATOM"
  in
  test "ac4694860f4a642899c6f147141d3a89"

module IoSample2 = struct
  open Effect.Deep
  open Js_of_ocaml
  open Lib.Effects

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
                          (fun w callback ->
                            Dom_html.setTimeout
                              (fun () ->
                                effects_log :=
                                  `Assoc
                                    [
                                      ( "query",
                                        Lib.Effects.query_params_to_yojson query
                                      );
                                    ]
                                  :: !effects_log;
                                with_effect effects_log query_stage (callback w)
                                  result)
                              10.0
                            |> ignore);
                      })
            | Fetch params ->
                Some
                  (fun (k : (a, _) continuation) ->
                    continue k
                      {
                        f =
                          (fun w callback ->
                            Dom_html.setTimeout
                              (fun () ->
                                effects_log :=
                                  `Assoc
                                    [
                                      ( "fetch",
                                        Lib.Effects.fetch_params_to_yojson
                                          params );
                                    ]
                                  :: !effects_log;
                                with_effect effects_log query_stage (callback w)
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
    !effects_log |> List.rev
    |> (function [ x ] -> x | xs -> `List xs)
    |> Yojson.Safe.pretty_to_string

  let assert_ sample stage =
    let effects_log = ref [] in
    let msg = Intergration_tests.read_sample sample in
    let effect : unit Io.t Io.t =
      with_effect effects_log stage Lib.Handler_bot.handle_message msg
    in
    effect.f { log = [] } (fun w e2 ->
        e2.f w (fun _w _ ->
            get_actual_effects_log effects_log
            |> Intergration_tests.assert_ ("expected." ^ sample)))

  let () = assert_ "sample1.json" "1"
  let () = assert_ "sample2.json" "1"
  let () = assert_ "sample3.json" "2"
  let () = assert_ "sample4.json" "1"
end

module XmlTests = struct
  let () =
    Intergration_tests.read_sample "ac4694860f4a642899c6f147141d3a89"
    |> Xml.parse_string |> Xml.children
    |> List.filter (fun x -> Xml.tag x = "entry")
    |> List.length |> string_of_int |> ignore;
    ()
end

module ScheduleTests = struct
  open Effect.Deep
  module U = Yojson.Safe.Util

  let test log_name =
    let log =
      Intergration_tests.read_sample log_name
      |> Yojson.Safe.from_string |> Yojson.Safe.Util.to_list |> ref
    in
    let rec run_effect : 'a 'b. ('a -> 'b) -> 'a -> 'b =
     fun f x ->
      try_with f x
        {
          effc =
            (fun (type a) (eff : a Effect.t) ->
              let open Lib.Effects in
              let entity = List.hd !log in
              log := List.tl !log;
              match eff with
              | QueryDbFx params ->
                  if "database" <> (U.member "name" entity |> U.to_string) then
                    failwith __LOC__;
                  if params |> query_params_to_yojson <> U.member "in" entity
                  then failwith __LOC__;
                  let result =
                    U.member "out" entity |> query_result_of_yojson
                    |> Result.get_ok
                  in
                  Some
                    (fun (k : (a, _) continuation) ->
                      continue k { f = (fun w c -> run_effect (c w) result) })
              | Fetch params ->
                  if "fetch" <> (U.member "name" entity |> U.to_string) then
                    failwith __LOC__;
                  if params |> fetch_params_to_yojson <> U.member "in" entity
                  then failwith __LOC__;
                  let result =
                    U.member "out" entity |> fetch_result_of_yojson
                    |> Result.get_ok
                  in
                  Some
                    (fun (k : (a, _) continuation) ->
                      continue k { f = (fun w c -> run_effect (c w) result) })
              | _ -> None);
        }
    in
    let ef = run_effect Lib.Handler_subscription.handle_ () in
    ef.f { log = [] } (fun w e2 -> e2.f w (fun _w _x -> print_endline "END"))

  let () = test "schedule1.json"
end

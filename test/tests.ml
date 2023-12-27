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
  let xml = Intergration_tests.read_sample "rss.xml" in
  let actual = Lib.Subscription_creator.is_atom xml in
  if not actual then failwith "Not RSS"

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
                                effects_log :=
                                  `Assoc
                                    [
                                      ( "query",
                                        Lib.Effects.query_params_to_yojson query
                                      );
                                    ]
                                  :: !effects_log;
                                with_effect effects_log query_stage callback
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
                          (fun callback ->
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
                                with_effect effects_log query_stage callback
                                  (`Ok ""))
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
      with_effect effects_log stage handle_message msg
    in
    effect.f (fun e2 ->
        e2.f (fun _ ->
            get_actual_effects_log effects_log
            |> Intergration_tests.assert_ ("expected." ^ sample)))

  let () = assert_ "sample1.json" "1"
  let () = assert_ "sample2.json" "1"
  let () = assert_ "sample3.json" "2"
  let () = assert_ "sample4.json" "1"
end

module ScheduleTests = struct
  open Effect.Deep

  let () =
    try_with Lib.Subscription_creator.get_new_subs ()
      {
        effc =
          (fun (type a) (eff : a Effect.t) ->
            match eff with
            | QueryDbFx p ->
                Lib.Effects.query_params_to_yojson p
                |> Yojson.Safe.pretty_to_string |> print_endline;
                Some (fun _ -> Io.never)
            | _ -> None);
      }
    |> ignore

  let () =
    try_with Lib.Subscription_creator.get_new_sub_contents
      [
        `Assoc
          [
            ("id", `Int 1);
            ("user_id", `String "2");
            ( "content",
              `String
                (`Assoc [ ("url", `String "https://g.com/") ]
                |> Yojson.Safe.to_string) );
          ];
      ]
      {
        effc =
          (fun (type a) (eff : a Effect.t) ->
            match eff with
            | Fetch p ->
                Lib.Effects.fetch_params_to_yojson p
                |> Yojson.Safe.pretty_to_string |> print_endline;
                Some (fun _ -> Io.never)
            | _ -> None);
      }
    |> ignore

  let () =
    try_with
      (Lib.Subscription_creator.save_subs
         [
           `Assoc
             [
               ("id", `Int 1);
               ("user_id", `String "2");
               ( "content",
                 `String
                   (`Assoc [ ("url", `String "https://g.com/") ]
                   |> Yojson.Safe.to_string) );
             ];
         ])
      [ `Ok {|<feed xmlns="http://www.w3.org/2005/Atom">|} ]
      {
        effc =
          (fun (type a) (eff : a Effect.t) ->
            match eff with
            | QueryDbFx p ->
                Lib.Effects.query_params_to_yojson p
                |> Yojson.Safe.pretty_to_string |> print_endline;
                Some (fun _ -> Io.never)
            | _ -> None);
      }
    |> ignore
end

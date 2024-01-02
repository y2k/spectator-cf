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

  let _assert sample_file_name actual =
    if sample_exists sample_file_name then (
      let expected = read_sample sample_file_name in
      if expected <> actual then
        TextComparer.compare_2_txt
          ("actual <> expected [" ^ sample_file_name ^ "]")
          expected actual)
    else write_sample sample_file_name actual
end

module ScheduleTests = struct
  open Lib.Effects
  module U = Yojson.Safe.Util

  let rec load_files (entity : Yojson.Safe.t) =
    match entity with
    | `String x when String.starts_with x ~prefix:"md5:" ->
        `String
          (String.sub x 4 (String.length x - 4)
          |> Intergration_tests.read_sample)
    | `List xs -> `List (xs |> List.map load_files)
    | `Assoc xs -> `Assoc (xs |> List.map (fun (k, v) -> (k, load_files v)))
    | x -> x

  let pretty_to_string_ex json =
    let rec expand_json = function
      | `String x when String.starts_with ~prefix:"{" x ->
          `Assoc [ ("<JSON>", Yojson.Safe.from_string x) ]
      | `List xs -> `List (List.map expand_json xs)
      | `Assoc xs -> `Assoc (List.map (fun (k, v) -> (k, expand_json v)) xs)
      | x -> x
    in
    json |> expand_json |> Yojson.Safe.pretty_to_string

  let debug_effect (log : Yojson.Safe.t list ref) p =
    {
      Io.f =
        (fun w callback ->
          let entity = List.hd !log in
          log := List.tl !log;
          let entity_without_out =
            `Assoc
              (entity |> Yojson.Safe.Util.to_assoc |> List.remove_assoc "out")
          in
          if p <> entity_without_out then
            Intergration_tests.TextComparer.compare_2_txt __LOC__
              (pretty_to_string_ex entity_without_out)
              (pretty_to_string_ex p);
          U.member "out" entity |> load_files |> callback w);
    }

  let test log_name (task : unit Io.t) =
    let log =
      Intergration_tests.read_sample log_name
      |> Yojson.Safe.from_string |> Yojson.Safe.Util.to_list |> ref
    in
    let ef = task in
    ef.f
      { perform = debug_effect log }
      (fun _w _x ->
        let count = List.length !log in
        if count > 0 then (
          prerr_endline @@ "[ERROR] is not empty: "
          ^ (`List !log |> Yojson.Safe.pretty_to_string);
          failwith "[ERROR]" |> ignore))

  let () = test "bot1.json" Lib.Handler_bot.handle
  let () = test "bot2.json" Lib.Handler_bot.handle
  let () = test "bot3.json" Lib.Handler_bot.handle
  let () = test "bot4.json" Lib.Handler_bot.handle
  let () = test "schedule1.json" Lib.Handler_subscription.handle
end

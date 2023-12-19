(* Intergration tests *)

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

let assert_ input_name =
  let json = read_sample input_name in
  let actual = ref "" in
  (decorate_with_mock_effect actual Lib.Spectator.handle_command json) ()
  |> ignore;
  let expected_filename = "expected." ^ input_name in
  if sample_exists expected_filename then (
    let expected = read_sample expected_filename in
    if expected <> !actual then failwith !actual)
  else write_sample expected_filename !actual

let () = assert_ "sample1.json"

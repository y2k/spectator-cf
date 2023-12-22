(* Effect sample *)

open Lib.Effect_handlers

type _ Effect.t +=
  | Fetch : string * Yojson.Safe.t -> Yojson.Safe.t Thunk.t Effect.t

type _ Effect.t += Database : Yojson.Safe.t -> Yojson.Safe.t Thunk.t Effect.t

let parse_req (_ : Yojson.Safe.t) : string * string = ("1", "/ls")
let make_ls_query (_user_id : string) : Yojson.Safe.t = `Assoc []
let empty_thunk : unit Thunk.t = fun _ -> Promise.Result.return ()

(**)

module Applicative = struct
  let pure (x : 'a) : 'a Thunk.t = fun () -> Promise.return (Ok x)

  let apply (mx : 'a Thunk.t) (mf : ('a -> 'b) Thunk.t) : 'b Thunk.t =
    let open Promise.Result.Syntax in
    fun () ->
      let* x = mx () in
      let+ f = mf () in
      f x
end

let thunk_flatten (ma : unit Thunk.t Thunk.t) : unit Thunk.t =
 fun () -> Promise.Result.bind (fun _f -> _f ()) (ma ())

let thunk_ignore (x : _ Thunk.t) : unit Thunk.t =
 fun () -> Promise.Result.bind (fun _ -> Promise.Result.return ()) (x ())

let handle_ls_completed (_ : Yojson.Safe.t) : unit Thunk.t =
  Effect.perform
    (Fetch
       ( "https://api.telegram.com/",
         `Assoc [ ("method", `String "POST"); ("body", `String "body") ] ))
  |> thunk_ignore

let handle_req (bot_req : Yojson.Safe.t) =
  let user_id, text = parse_req bot_req in
  match String.split_on_char ' ' text with
  | [ "/ls" ] ->
      Applicative.pure handle_ls_completed
      |> Applicative.apply (Effect.perform (Database (make_ls_query user_id)))
      |> thunk_flatten
  | _ -> empty_thunk

let _ =
  let open Promise.Syntax in
  let open Effect.Deep in
  let* _a =
    (try_with handle_req (`Assoc [])
       {
         effc =
           (fun (type a) (eff : a Effect.t) ->
             match eff with
             | Database _props ->
                 Some
                   (fun (k : (a, _) continuation) ->
                     continue k (Applicative.pure (`Assoc [])))
             | Fetch (_url, _props) ->
                 Some
                   (fun (k : (a, _) continuation) ->
                     continue k (Applicative.pure (`Assoc [])))
             | _ -> None);
       })
      ()
    |> Promise.Result.from_catch
  in
  match _a with
  | Ok _ -> Promise.return ()
  | Error _e ->
      trace "Error" _e |> ignore;
      Promise.return ()

(* Intergration tests *)

(* open Lib.Effect_handlers

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

   let () =
     assert_ "sample2.json";
     assert_ "sample3.json" *)

(* module Intergration_tests = struct
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

     let () =
       assert_ "sample2.json";
       assert_ "sample3.json"
   end *)

module EffectSample = struct
  open Effect
  open Effect.Deep

  module Promise = struct
    open Js_of_ocaml

    type 'a t = private Js.Unsafe.any

    let map (f : 'a -> 'b) (p : 'a t) : 'b t =
      Js.Unsafe.meth_call p "then" [| Js.Unsafe.inject f |]

    (* let return (x : 'a) : 'a t =
       Js.Unsafe.meth_call
         (Js.Unsafe.pure_js_expr "Promise")
         "resolve"
         [| Js.Unsafe.inject x |] *)

    let make (f : ('a -> unit) -> unit) : 'a t =
      Js.Unsafe.new_obj
        (Js.Unsafe.pure_js_expr "Promise")
        [|
          (* Js.Unsafe.inject f  *)
          Js.Unsafe.inject
            (Js.Unsafe.callback (fun resolve ->
                 f (fun x ->
                     Lib.Effect_handlers.trace "1) Promise.make"
                       (Js.Unsafe.inject resolve)
                     |> ignore;
                     Lib.Effect_handlers.trace "2) Promise.make"
                       (Js.Unsafe.inject x)
                     |> ignore;
                     resolve x)));
        |]
  end

  type world = World
  type 'a thunk = { f : world -> 'a Promise.t }

  let _ignore_this (x : int thunk) =
    let _ = x.f World in
    ()

  type _ Effect.t += E2 : string -> Yojson.Safe.t thunk Effect.t

  (* let ( let+ ) (ma : 'a thunk) (f : 'a -> 'b) : 'b thunk =
     { f = (fun w -> Promise.map f (ma.f w)) } *)

  (* ======================================================================== *)
  (* ======================================================================== *)

  let foo x =
    print_endline "1.1";
    let b1 = perform (E2 x) in
    Lib.Effect_handlers.trace "1.1.5" b1 |> ignore;
    (* let+ _b = b1 in
       Lib.Effect_handlers.trace "1.2" _b |> ignore;
       print_endline @@ "foo) " ^ (_b |> Yojson.Safe.pretty_to_string);
       print_endline "1.3";
       _b *)
    {
      f =
        (fun (w : world) ->
          b1.f w
          |> Promise.map (fun _b ->
                 Lib.Effect_handlers.trace "1.2" _b |> ignore;
                 print_endline @@ "foo) " ^ (_b |> Yojson.Safe.pretty_to_string);
                 print_endline "1.3";
                 _b));
    }

  (* let bar () =
     print_endline "1";
     let+ _c = foo "2" in
     print_endline @@ "bar) " ^ (_c |> Yojson.Safe.pretty_to_string);
     _c *)

  let effect_handler =
    {
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | E2 _x ->
              Some
                (fun (k : (a, _) continuation) ->
                  {
                    f =
                      (fun _world ->
                        let x = `Assoc [ ("key", `String _x) ] in
                        Lib.Effect_handlers.trace "4.0" x |> ignore;
                        (* let _r = x |> Promise.return *)
                        let _r =
                          Promise.make (fun resolve ->
                              Lib.Effect_handlers.trace "4.1" x |> ignore;
                              Lib.Effect_handlers.trace "4.2"
                                (Js_of_ocaml.Js.Unsafe.inject resolve)
                              |> ignore;
                              resolve x;
                              Lib.Effect_handlers.trace "4.2" x |> ignore)
                          |> Promise.map (fun x ->
                                 Lib.Effect_handlers.trace "4.4" x)
                        in
                        Lib.Effect_handlers.trace "4.3" _r |> ignore;
                        _r);
                  }
                  |> continue k)
          | _ -> None);
    }

  let _ignore () =
    let _result = try_with foo "INPUT" effect_handler in
    let _a = _result.f World in
    Lib.Effect_handlers.trace "3" _a |> ignore;
    _a |> Promise.map (fun x -> Lib.Effect_handlers.trace "5.1" x) |> ignore;
    input_line stdin |> ignore;
    ()
end

module IoSample = struct
  type world = World
  type 'a io = { f : world -> ('a -> unit) -> unit }

  let map (f : 'a -> 'b) (ma : 'a io) : 'b io =
    { f = (fun w callback -> ma.f w (fun x -> callback (f x))) }

  let _pure x = { f = (fun _ callback -> callback x) }

  let combine2 ma mb =
    {
      f =
        (fun w callback -> ma.f w (fun a -> mb.f w (fun b -> callback (a, b))));
    }

  module Syntax = struct
    let ( let+ ) ma f = map f ma
  end

  type _ Effect.t += E2 : int -> string io Effect.t

  let foo x =
    let open Syntax in
    let ma = Effect.perform (E2 (10 * x)) in
    let mb = Effect.perform (E2 x) in
    let+ a, b = combine2 ma mb in
    a ^ "/" ^ b

  open Effect.Deep

  let effect_handler =
    {
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | E2 _x ->
              Some
                (fun (k : (a, _) continuation) ->
                  let open Js_of_ocaml in
                  continue k
                    {
                      f =
                        (fun _w callback ->
                          Dom_html.setTimeout
                            (fun () -> callback (string_of_int _x))
                            10.0
                          |> ignore);
                    })
          | _ -> None);
    }

  let _ignore () =
    let a = try_with foo 7 effect_handler in
    a.f World print_endline
end

module IoSample2 = struct
  module Io = struct
    type 'a t = { f : ('a -> unit) -> unit }

    let map (f : 'a -> 'b) (ma : 'a t) : 'b t =
      { f = (fun callback -> ma.f (fun x -> callback (f x))) }

    let pure x = { f = (fun callback -> callback x) }

    let combine2 ma mb =
      { f = (fun callback -> ma.f (fun a -> mb.f (fun b -> callback (a, b)))) }

    let ignore ma = { f = (fun callback -> ma.f (fun _ -> callback ())) }

    module Syntax = struct
      let ( let+ ) ma f = map f ma

      module Monad = struct
        let ( let* ) (ma : 'a t) (f : 'a -> 'b t) : 'b t =
          {
            f =
              (fun callback ->
                ma.f (fun a ->
                    let mb = f a in
                    mb.f callback));
          }
      end
    end
  end

  type _ Effect.t +=
    | Fetch : Yojson.Safe.t -> Yojson.Safe.t Io.t Effect.t
    | QueryDbFx : Yojson.Safe.t -> string list Io.t Effect.t

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
                  Some
                    (fun (k : (a, _) continuation) ->
                      continue k
                        {
                          f =
                            (fun callback ->
                              Dom_html.setTimeout
                                (fun () ->
                                  query |> Yojson.Safe.pretty_to_string
                                  |> Printf.sprintf "=== Query DB ===\n%s"
                                  |> print_endline;
                                  with_effect callback
                                    [ {|{ url: "https://g.com" }|} ])
                                10.0
                              |> ignore);
                        })
              | Fetch req ->
                  Some
                    (fun (k : (a, _) continuation) ->
                      continue k
                        {
                          f =
                            (fun callback ->
                              Dom_html.setTimeout
                                (fun () ->
                                  req |> Yojson.Safe.pretty_to_string
                                  |> Printf.sprintf "=== Fetch ===\n%s"
                                  |> print_endline;
                                  with_effect callback `Null)
                                10.0
                              |> ignore);
                        })
              | _ -> None);
        }

  let handle_ls_command user_id =
    let open Io.Syntax in
    let+ subs =
      Effect.perform
        (QueryDbFx
           (`Assoc
             [
               ( "sql",
                 `String "SELECT * FROM new_subscriptions WHERE user_id = ?" );
               ("params", `List [ `String user_id ]);
             ]))
    in
    let response_msg =
      subs |> List.fold_left (Printf.sprintf "%s\n%s") "Subscriptions:"
    in
    Effect.perform
      (Fetch
         (`Assoc
           [
             ("url", `String "https://api.telegram.com/bot__TOKEN__/sendMessage");
             ("method", `String "POST");
             ("headers", `Assoc [ ("Content-Type", `String "application/json") ]);
             ( "body",
               `Assoc
                 [
                   ("user_id", `String user_id);
                   ("message", `String response_msg);
                 ] );
           ]))
    |> Io.ignore

  let handle_add_command user_id url =
    let open Io.Syntax in
    let response_msg = "Subscription added" in
    let f1 =
      Effect.perform
        (QueryDbFx
           (`Assoc
             [
               ( "sql",
                 `String
                   "INSERT INTO new_subsciriptions (user_id, url) VALUE (?, ?)"
               );
               ("params", `List [ `String user_id; `String url ]);
             ]))
    in
    let f2 =
      Effect.perform
        (Fetch
           (`Assoc
             [
               ( "url",
                 `String "https://api.telegram.com/bot__TOKEN__/sendMessage" );
               ("method", `String "POST");
               ( "headers",
                 `Assoc [ ("Content-Type", `String "application/json") ] );
               ( "body",
                 `Assoc
                   [
                     ("user_id", `String user_id);
                     ("message", `String response_msg);
                   ] );
             ]))
    in
    let+ _ = Io.pure () in
    Io.combine2 f1 f2 |> Io.ignore

  let handle_message message =
    let user_id = Fun.const "42" message in
    match message |> String.split_on_char ' ' with
    | [ "/ls" ] -> handle_ls_command user_id
    | [ "/add"; url ] -> handle_add_command user_id url
    | _ -> failwith __LOC__

  let _ =
    let a : unit Io.t Io.t = with_effect handle_message "/add https://g.com/" in
    a.f (fun a ->
        a.f (fun _ ->
            print_endline "================================================";
            let a = with_effect handle_message "/ls" in
            a.f (fun a -> a.f (fun _ -> ())) |> ignore))
end

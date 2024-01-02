open Js_of_ocaml
open Lib.Effects

let execute_handle text env (handle_message : unit Io.t) =
  Promise.make (fun ~resolve ~reject:_ ->
      let w : Io.world =
        { perform = Io.unhandled } |> Impl.attach_db_effect env
        |> Impl.attach_fetch_effect env
        |> Impl.attach_const_effect text
        |> Impl.attach_log_effect
      in
      handle_message.f w (fun _ -> resolve))

let () =
  Js.export "fetch" (fun request env _ctx ->
      Promise.return ()
      |> Promise.then_ ~fulfilled:(fun _ ->
             let open Promise.Syntax in
             let* (text : string) = request##text in
             execute_handle text env Lib.Handler_bot.handle)
      |> Promise.catch ~rejected:(fun e ->
             Firebug.console##warn e |> Promise.return)
      |> Promise.then_ ~fulfilled:(fun _ ->
             Js.Unsafe.new_obj
               (Js.Unsafe.pure_js_expr "Response")
               [| Js.Unsafe.inject "OK" |]
             |> Promise.return));
  Js.export "scheduled" (fun _event env ctx ->
      ctx##waitUntil
        (Promise.return ()
        |> Promise.then_ ~fulfilled:(fun _ ->
               execute_handle "" env Lib.Handler_subscription.handle)
        |> Promise.catch ~rejected:(fun e ->
               Firebug.console##warn e |> Promise.return)
        |> Promise.then_ ~fulfilled:(fun _ -> Promise.return ())))

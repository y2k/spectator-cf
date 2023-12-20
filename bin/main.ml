open Js_of_ocaml

let () =
  Js.export "fetch" (fun request env _ctx ->
      Promise.return ()
      |> Promise.then_ ~fulfilled:(fun _ ->
             Lib.Spectator.handle_fetch request env)
      |> Promise.catch ~rejected:(fun e ->
             Firebug.console##warn e |> Promise.return)
      |> Promise.then_ ~fulfilled:(fun _ ->
             Js.Unsafe.new_obj
               (Js.Unsafe.pure_js_expr "Response")
               [| Js.Unsafe.inject "OK" |]
             |> Promise.return))

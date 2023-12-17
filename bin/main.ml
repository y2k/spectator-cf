open Js_of_ocaml

let _ =
  Dom.addEventListener Js.Unsafe.global (Dom.Event.make "fetch")
    (Dom.handler (fun e ->
         e##respondWith
           (Promise.return ()
           |> Promise.then_ ~fulfilled:(fun _ ->
                  Spectator.handle_fetch e##.request)
           |> Promise.catch ~rejected:(fun e ->
                  Firebug.console##warn e |> Promise.return)
           |> Promise.then_ ~fulfilled:(fun _ ->
                  Js.Unsafe.new_obj
                    (Js.Unsafe.pure_js_expr "Response")
                    [| Js.Unsafe.inject "OK" |]
                  |> Promise.return))))
    Js._false

module Debug = struct
  open Js_of_ocaml.Js

  let trace prefix x =
    Unsafe.meth_call
      (Unsafe.pure_js_expr "console")
      "log"
      [| Unsafe.inject prefix; Unsafe.inject x |]
    |> ignore;
    x
end

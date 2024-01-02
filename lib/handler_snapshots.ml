open Effects

let _handle =
  let open Io.Syntax in
  let* _subs = Effects.query ("SELECT * FROM subscriptions LIMIT 5", []) in
  Io.pure ()

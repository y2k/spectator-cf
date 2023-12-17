let run_test (assert_f : (string * string -> string -> unit) -> unit) =
  let handle_command = Spectator.Handler.handle_command () in
  assert_f (fun (user, message) expected ->
      let result = handle_command user message in
      if result <> expected then failwith result)

let () = run_test (fun assert_ -> assert_ ("1", "/ls") "No subscriptions")

let () =
  run_test (fun assert_ ->
      assert_ ("1", "/add https://g.com/") "Subscription added";
      assert_ ("1", "/ls") "Subscriptions:\n- https://g.com/")

type sexp = String of string | Int of int | List of sexp list

let _ =
  (*
     (fetch
       "https://api.telegram.org/bot"
       ((method "POST")
        (headers ("Content-Type" "application/json"))
        (body
         (chat_id 0)
         (text "hello world")))))

     [:fetch "https://api.telegram.org/bot"
             {:method "POST"
              :headers {:content-type "application/json"}
              :body {:chat_id 0 :text "hello world"}}]

*)
  List
    [
      String "fetch";
      String "https://api.telegram.org/bot";
      List
        [
          List [ String "method"; String "POST" ];
          List
            [
              String "headers";
              List [ List [ String "Content-Type"; String "application/json" ] ];
            ];
          List
            [
              String "body";
              List
                [
                  List [ String "chat_id"; Int 0 ];
                  List [ String "text"; String "hello world" ];
                ];
            ];
        ];
    ]

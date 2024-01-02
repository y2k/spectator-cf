
-- SELECT * FROM new_subscriptions
SELECT content->>'url' FROM subscriptions
-- WHERE content->>'user_id' = '241854720'

/*

((select id (content->'$' content))
 (from new_subscriptions)
 (where (= content->'$' content)))

*/

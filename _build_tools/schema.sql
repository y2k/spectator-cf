DROP TABLE IF EXISTS new_subscriptions;

CREATE TABLE IF NOT EXISTS new_subscriptions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    -- user_id TEXT NOT NULL,
    content TEXT NOT NULL
);

DROP TABLE IF EXISTS subscriptions;

CREATE TABLE IF NOT EXISTS subscriptions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    -- user_id TEXT NOT NULL,
    content TEXT NOT NULL
);

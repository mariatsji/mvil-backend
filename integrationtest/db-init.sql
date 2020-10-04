CREATE TABLE posts (
    post_id serial NOT NULL,
    "user" TEXT not null,
    post TEXT not null,
    timestamp timestamptz NOT NULL default now(),
    PRIMARY KEY (post_id)
);

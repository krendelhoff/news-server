CREATE EXTENSION "uuid-ossp";

CREATE TABLE users
( id uuid PRIMARY KEY DEFAULT uuid_generate_v4()
, name text NOT NULL
, surname text NOT NULL
, login text NOT NULL
, avatar uuid
, password_hash text NOT NULL
, created_at timestamp with time zone NOT NULL DEFAULT now()
, privileged bool NOT NULL DEFAULT false
, UNIQUE (login, password_hash)
);

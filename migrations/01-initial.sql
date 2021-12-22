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

CREATE TABLE pictures
( id uuid PRIMARY KEY DEFAULT uuid_generate_v4()
, picture bytea NOT NULL
);

CREATE TABLE auth
( token text NOT NULL
, expires timestamp with time zone NOT NULL
, user_id uuid NOT NULL REFERENCES users(id)
, privileged bool NOT NULL
, UNIQUE (user_id)
);

CREATE TABLE authors
( id uuid PRIMARY KEY DEFAULT uuid_generate_v4()
, user_id uuid NOT NULL REFERENCES users(id)
, description text
);

CREATE TABLE categories
( id uuid PRIMARY KEY DEFAULT uuid_generate_v4()
, title text NOT NULL
);

CREATE TABLE categories_content
( category uuid NOT NULL REFERENCES categories(id)
, subcategory uuid NOT NULL REFERENCES categories(id)
);

CREATE TABLE tags
( id uuid PRIMARY KEY DEFAULT uuid_generate_v4()
, title text NOT NULL
);

CREATE TABLE news
( id uuid PRIMARY KEY DEFAULT uuid_generate_v4()
, title text NOT NULL
, created_at timestamp with time zone NOT NULL DEFAULT now()
, author uuid NOT NULL REFERENCES authors(id)
, category uuid NOT NULL REFERENCES categories(id)
, photo bytea NOT NULL
);

CREATE TABLE post_photos
( post uuid NOT NULL REFERENCES news(id)
, photo bytea NOT NULL
);

CREATE TABLE post_tags
( post uuid NOT NULL REFERENCES news(id)
, tag uuid NOT NULL REFERENCES tags(id)
);

CREATE TABLE comments
( post uuid NOT NULL REFERENCES news(id)
, comment text NOT NULL
);

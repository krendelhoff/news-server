CREATE EXTENSION "uuid-ossp";

CREATE TABLE pictures
( id uuid PRIMARY KEY DEFAULT uuid_generate_v4()
, picture bytea NOT NULL
);

CREATE TABLE users
( id uuid PRIMARY KEY DEFAULT uuid_generate_v4()
, name text NOT NULL
, surname text NOT NULL
, login text NOT NULL
, avatar uuid REFERENCES pictures(id)
, password_hash text NOT NULL
, created_at timestamp with time zone NOT NULL DEFAULT now()
, privileged bool NOT NULL DEFAULT false
, UNIQUE (login, password_hash)
);

CREATE TABLE auth
( token text NOT NULL
, expires timestamp with time zone NOT NULL
, user_id uuid PRIMARY KEY REFERENCES users(id)
, privileged bool NOT NULL DEFAULT false
, refresh_token text NOT NULL
);

CREATE TABLE authors
( user_id uuid PRIMARY KEY REFERENCES users(id)
, description text
);

CREATE TABLE categories
( id uuid PRIMARY KEY DEFAULT uuid_generate_v4()
, title text NOT NULL
, supercategory uuid
, UNIQUE (title, supercategory)
, CONSTRAINT cat_check CHECK ((supercategory = NULL::uuid) = (title = '/'::text))
);
INSERT INTO categories (title, supercategory) VALUES ('/', null);

CREATE TABLE tags
( id uuid PRIMARY KEY DEFAULT uuid_generate_v4()
, title text NOT NULL
);

CREATE TABLE news
( id uuid PRIMARY KEY DEFAULT uuid_generate_v4()
, title text NOT NULL
, created_at timestamp with time zone NOT NULL DEFAULT now()
, author uuid NOT NULL REFERENCES authors(user_id)
, category uuid NOT NULL REFERENCES categories(id)
, photo uuid NOT NULL REFERENCES pictures(id)
);

CREATE TABLE post_photos
( post uuid NOT NULL
, photo uuid NOT NULL REFERENCES pictures(id)
);
CREATE INDEX post_photos_index ON post_photos(post);

CREATE TABLE post_tags
( post uuid NOT NULL
, tag uuid NOT NULL REFERENCES tags(id)
);
CREATE INDEX post_tags_index ON post_tags(post);

CREATE TABLE comments
( id uuid PRIMARY KEY DEFAULT uuid_generate_v4()
, post uuid NOT NULL REFERENCES news(id)
, comment text NOT NULL
);

CREATE TABLE post_drafts
( id uuid PRIMARY KEY DEFAULT uuid_generate_v4()
, post uuid NOT NULL REFERENCES news(id)
, title text NOT NULL
, modified timestamp with time zone NOT NULL DEFAULT now()
, category uuid NOT NULL REFERENCES categories(id)
, photo uuid NOT NULL REFERENCES pictures(id)
);

{-# LANGUAGE QuasiQuotes #-}
module Migration.Initial where

import Database.Persist.Migration
import Text.RawString.QQ
import Universum

createComments :: Operation
createComments = RawOperation "Creates comments table" $
  return [MigrateSql createCommentsMigration []]

createCommentsMigration :: Text
createCommentsMigration =
  [r| CREATE TABLE comments (
        post uuid NOT NULL REFERENCES news(id)
      , comment text NOT NULL
      );
  |]

createPostTags :: Operation
createPostTags = RawOperation "Creates post_tags table" $
  return [MigrateSql createPostTagsMigration []]

createPostTagsMigration :: Text
createPostTagsMigration =
  [r| CREATE TABLE post_tags (
        post uuid NOT NULL REFERENCES news(id)
      , tag uuid NOT NULL REFERENCES tags(id)
      );
  |]

createPostPhotos :: Operation
createPostPhotos = RawOperation "Creates post_photos table" $
  return [MigrateSql createPostPhotosMigration []]

createPostPhotosMigration :: Text
createPostPhotosMigration =
  [r| CREATE TABLE post_photos (
        post uuid NOT NULL REFERENCES news(id)
      , photo bytea NOT NULL
      );
  |]

createNews :: Operation
createNews = RawOperation "Creates news table" $
  return [MigrateSql createNewsMigration []]

createNewsMigration :: Text
createNewsMigration =
  [r| CREATE TABLE news (
        id uuid PRIMARY KEY DEFAULT uuid_generate_v4()
      , title text NOT NULL
      , created_at timestamp with time zone NOT NULL DEFAULT now()
      , author uuid NOT NULL REFERENCES users(id)
      , category uuid NOT NULL REFERENCES categories(id)
      , photo bytea NOT NULL
      );
  |]

createTags :: Operation
createTags = RawOperation "Creates tags table" $
  return [MigrateSql createTagsMigration []]

createTagsMigration :: Text
createTagsMigration =
  [r| CREATE TABLE tags (
        id uuid PRIMARY KEY DEFAULT uuid_generate_v4()
      , title text NOT NULL
      );
  |]

createCategoriesContent :: Operation
createCategoriesContent = RawOperation "Creates categories_content table" $
  return [MigrateSql createCategoriesContentMigration []]

createCategoriesContentMigration :: Text
createCategoriesContentMigration =
  [r| CREATE TABLE categories_content (
        category uuid NOT NULL REFERENCES categories(id)
      , subcategory uuid NOT NULL REFERENCES categories(id)
      );
  |]

createCategories :: Operation
createCategories = RawOperation "Creates categories table" $
  return [MigrateSql createCategoriesMigration []]

createCategoriesMigration :: Text
createCategoriesMigration =
  [r| CREATE TABLE categories (
        id uuid PRIMARY KEY DEFAULT uuid_generate_v4()
      , title text NOT NULL
      );
  |]

createAuthors :: Operation
createAuthors = RawOperation "Creates authors table" $
  return [MigrateSql createAuthorsMigration []]

createAuthorsMigration :: Text
createAuthorsMigration =
  [r| CREATE TABLE authors (
        user_id uuid NOT NULL REFERENCES users(id)
      , description text
      );
  |]

createAuth :: Operation
createAuth = RawOperation "Creates auth table" $
  return [MigrateSql createAuthMigration []]

createAuthMigration :: Text
createAuthMigration =
  [r| CREATE TABLE auth (
        token text NOT NULL
      , created_at timestamp with time zone NOT NULL DEFAULT now()
      , user_id uuid NOT NULL REFERENCES users(id)
      );
  |]

createUsers :: Operation
createUsers = RawOperation "Creates users table" $
  return [MigrateSql createUsersMigration []]

createUsersMigration :: Text
createUsersMigration =
  [r| CREATE TABLE users (
      id uuid PRIMARY KEY DEFAULT uuid_generate_v4()
      , name text NOT NULL
      , surname text NOT NULL
      , login text NOT NULL
      , avatar bytea
      , password_hash text NOT NULL
      , created_at timestamp with time zone NOT NULL DEFAULT now()
      , privileged bool NOT NULL DEFAULT false
      );
  |]

addUUIDExtension :: Operation
addUUIDExtension = RawOperation "Adds UUID Extension" $
  return [MigrateSql "CREATE EXTENSION \"uuid-ossp\"" []]

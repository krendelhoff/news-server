{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}
module Database.Categories where

import Universum hiding (get, toText)
import Data.Coerce (coerce)

import Infrastructure
import Types.Categories

import qualified Data.Vector as V

decodePayload :: (UUID, Text, Maybe UUID) -> Payload
decodePayload ( fromUUID -> cid
              , fromText -> title
              , (fromUUID <$>) -> supercat
              ) = Payload cid title supercat

get :: ID -> Transaction (Maybe Payload)
get (toUUID -> cid) = (decodePayload <$>) <$>
  statement cid [maybeStatement|
  SELECT id::uuid,title::text,supercategory::uuid?
  FROM categories
  WHERE id = $1::uuid
                                    |]

remove :: ID -> Transaction ()
remove (toUUID -> cid) = statement cid [resultlessStatement|
  DELETE FROM categories WHERE id=$1::uuid
                                       |]

isItUnique :: UUID -> Text -> Maybe UUID -> Transaction Bool
isItUnique _ "/" _ = return False
isItUnique root title mSuperCat =
  not <$> statement (title, fromMaybe root mSuperCat)
  [singletonStatement|
    SELECT EXISTS( SELECT * FROM categories
                   WHERE title=$1::text AND supercategory = $2::uuid
                  )::bool
  |]

create :: Title -> Maybe ID -> Transaction (Maybe Payload)
create (toText -> title) ((toUUID <$>) -> mSupercat) = do
  root <- toUUID <$> rootID
  isItUnique root title mSupercat >>= \case
    True -> createCat title mSupercat <&> Just . decodePayload
                                               . (,title,mSupercat)
    False -> return Nothing
  where
    createCat title mSuperCat = do
      supercat <- maybe (toUUID <$> rootID) return mSuperCat
      statement (title, supercat)
        [singletonStatement|
          INSERT INTO categories (title, supercategory)
          VALUES ($1::text,$2::uuid)
          RETURNING id::uuid
        |]


rename :: ID -> Title -> Transaction (Maybe Payload)
rename cat@(toUUID -> cid) (toText -> title) = do
  Payload _ _ ((toUUID <$>) -> mSupercat) <- getUnsafe cat
  root <- toUUID <$> rootID
  isItUnique root title mSupercat >>= \case
    False -> return Nothing
    True  -> Just . decodePayload <$>
      statement (cid, title) [singletonStatement|
        UPDATE categories SET title=$2::text WHERE id=$1::uuid
        RETURNING id::uuid,title::text,supercategory::uuid?
                             |]

rootID :: Transaction ID
rootID = fromUUID <$> statement () [singletonStatement|
  SELECT id::uuid FROM categories WHERE title='/'
                                   |]

getPathToRoot :: ID -> Transaction (Vector ID)
getPathToRoot (toUUID -> cid) = coerce <$>
  statement cid [vectorStatement|
    WITH RECURSIVE cats as
    ( SELECT c.supercategory,c.id
      FROM categories c
      WHERE c.id=$1::uuid
      UNION
      SELECT c.supercategory,c.id
      FROM categories c
      INNER JOIN cats ON (cats.supercategory = c.id)
    ) SELECT id::uuid FROM cats
                |]

getUnsafe :: ID -> Transaction Payload
getUnsafe (toUUID -> cid) = decodePayload <$>
  statement cid [singletonStatement|
  SELECT id::uuid,title::text,supercategory::uuid?
  FROM categories
  WHERE id = $1::uuid
                |]

getRecursive :: ID -> Transaction (Maybe PayloadRecursive)
getRecursive = get >=> \case
  Nothing -> return Nothing
  Just (Payload cid _ _) -> do
    cats <- getPathToRoot cid
    let base = getUnsafe (V.last cats) <&>
          \(Payload ct title _) -> PayloadRecursive ct title Nothing
    Just <$>
      foldr (\x accM -> do
                acc <- accM
                Payload ct title _ <- getUnsafe x
                return $ PayloadRecursive ct title $ Just acc
            ) base (V.init cats)

getAllChilds :: ID -> Transaction (Vector ID)
getAllChilds (toUUID -> cid) = coerce <$>
  statement cid [vectorStatement|
    WITH RECURSIVE cats as
    ( SELECT c.supercategory,c.id
      FROM categories c
      WHERE c.id=$1::uuid
      UNION
      SELECT c.supercategory,c.id
      FROM categories c
      INNER JOIN cats ON (cats.id = c.supercategory)
    ) SELECT id::uuid FROM cats
                |]

rebase :: ID -- ^ What
       -> ID -- ^ Where
       -> Transaction (Maybe Payload)
rebase whatC whereC = do
  childs <- getAllChilds whatC
  if whereC `V.elem` childs
  then return Nothing
  else Just . decodePayload <$> updateSupercategory whatC whereC
  where
    updateSupercategory (toUUID -> cat) (toUUID -> supercat) =
      statement (cat, supercat) [singletonStatement|
        UPDATE categories SET supercategory=$2::uuid
        WHERE id=$1::uuid
        RETURNING id::uuid,title::text,supercategory::uuid?
                                |]

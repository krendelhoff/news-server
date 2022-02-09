{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}
module Database.Categories where

import Universum hiding (get, toText)
import Data.Coerce (coerce)

import Infrastructure
import Types.Categories

import qualified Data.Vector as V

encodePayload :: (UUID, Text, Maybe UUID) -> Payload
encodePayload ( fromUUID -> cid
              , fromText -> title
              , (fromUUID <$>) -> supercat
              ) = Payload cid title supercat

get :: ID -> Transaction (Maybe Payload)
get (toUUID -> cid) = (encodePayload <$>) <$>
  statement cid [maybeStatement|
  SELECT id::uuid,title::text,supercategory::uuid?
  FROM categories
  WHERE id = $1::uuid
                                    |]

remove :: ID -> Transaction NoContent
remove (toUUID -> cid) = NoContent <$ statement cid [resultlessStatement|
  DELETE FROM categories WHERE id=$1::uuid
                                                    |]

isItUnique :: Text -> Maybe UUID -> Transaction Bool
isItUnique title mSupercat = not <$> statement (title, mSupercat)
  [singletonStatement|
    SELECT EXISTS( SELECT * FROM categories
                   WHERE title=$1::text AND ((supercategory IS NULL
                                          AND $2::uuid? IS NULL) OR
                                              supercategory=$2::uuid?
                                            )
                  )::bool
  |]

create :: Title -> Maybe ID -> Transaction (Maybe Payload)
create (toText -> title) ((toUUID <$>) -> mSupercat) = do
  isItUnique title mSupercat >>= \case
    True -> createCat title mSupercat <&> Just . encodePayload
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
  isItUnique title mSupercat >>= \case
    False -> return Nothing
    True  -> Just . encodePayload <$>
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
getUnsafe (toUUID -> cid) = encodePayload <$>
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
  else Just . encodePayload <$> updateSupercategory whatC whereC
  where
    updateSupercategory (toUUID -> cat) (toUUID -> supercat) =
      statement (cat, supercat) [singletonStatement|
        UPDATE categories SET supercategory=$2::uuid
        WHERE id=$1::uuid
        RETURNING id::uuid,title::text,supercategory::uuid?
                                |]

{-# LANGUAGE QuasiQuotes  #-}
{-# LANGUAGE ViewPatterns #-}
module Database.Auth where

import Hasql.TH
import Hasql.Transaction
import Universum         hiding (toText)

import Types.Auth
import Types.Common
import Types.TH
import Types.Users

getTokenInfo :: Token -> CurrentTime -> Transaction (Maybe TokenInfo)
getTokenInfo mToken (toUTCTime -> curTime) =
  (encodeTokenInfo <$>) <$> statement (toText mToken)
  [maybeStatement| SELECT token::text, privileged::bool, expires::timestamptz
                        , user_id::uuid
                   FROM auth WHERE token=$1::text |]
  where
    encodeTokenInfo ( fromText -> token
                    , fromBool -> privileged
                    , fromBool . (>= curTime) -> expired
                    , fromUUID -> user
                    ) = TokenInfo token privileged expired user

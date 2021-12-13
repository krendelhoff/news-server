{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes  #-}
module Database.Auth where

import Universum hiding (toText)
import Hasql.Transaction
import Hasql.TH

import Types.Auth
import Types.TH

getTokenInfo :: Token -> CurrentTime -> Transaction (Maybe TokenInfo)
getTokenInfo mToken (toUTCTime -> curTime) =
  (encodeTokenInfo <$>) <$> statement (toText mToken)
  [maybeStatement| SELECT token::text, privileged::bool, expires::timestamptz
                   FROM auth WHERE token=$1::text |]
  where
    encodeTokenInfo ( fromText -> token
                    , fromBool -> privileged
                    , fromBool . (>= curTime) -> expired
                    ) = TokenInfo token privileged expired

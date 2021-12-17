module Types.Errors ( throwError
                    , status404
                    , status403
                    , status401
                    ) where

import Network.HTTP.Types
import Control.Monad.Except

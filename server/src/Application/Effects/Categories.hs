module Application.Effects.Categories where

import Universum

import Application.Effects
import Infrastructure
import Types.Categories

class AuthenticateUser m => AcquireCategory m where
  get          :: ID -> m (Maybe Payload)
  getRecursive :: ID -> m (Maybe PayloadRecursive)

-- NOTE design decision - handle db invariants and exceptions at level of that functions
-- that's why all maybe - categories are complex entity
class (AcquireCategory m, AuthenticateAdmin m) => PersistCategory m where
  create :: Title -> Maybe ID -> m (Maybe Payload)
  remove :: ID -> m NoContent
  rename :: ID -> Maybe ID -> Title -> m (Maybe Payload)
  rebase :: ID -> ID -> m (Maybe Payload)

-- | W2.1 Roulette
module W0201
    ( -- * Color
      Color (..)
      -- * Subtask W2.1.1
    , roulette
      -- * Subtask W2.1.2
    , gamblersRuin
    ) where

import Numeric.Natural (Natural)
import System.Random

data Color = Zero | Red | Black
    deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- Subtask W2.1.1

roulette :: IO Color
roulette = error "TODO: implement roulette"

-- Subtask W2.1.2

-- |
-- >>> gamblersRuin 0
-- 0
--
gamblersRuin :: Natural -> IO Natural
gamblersRuin = error "TODO: implement gamblersRuin"

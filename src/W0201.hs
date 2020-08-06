-- | W2.1 Roulette
module W0201
    ( -- * Color
      Color (..)
      -- * Subtask W2.1.1
    , roulette
      -- * Subtask W2.1.2
    , histogram
      -- * Subtask W2.1.3
    , gamblersRuin
    ) where

import Data.Map (Map)
import System.Random

data Color = Zero | Red | Black
    deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- Subtask W2.1.1

roulette :: IO Color
roulette = error "TODO: implement roulette"

-- Subtask W2.1.2

-- |
-- >>> histogram 0
-- fromList []
--
histogram :: Int -> IO (Map Color Int)
histogram = error "TODO: implement histogram"

-- Subtask W2.1.3

-- |
-- >>> gamblersRuin 0
-- 0
--
gamblersRuin :: Int -> IO Int
gamblersRuin = error "TODO: implement gamblersRuin"

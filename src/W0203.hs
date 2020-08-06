-- | W2.3 Unsafe IO
module W0203
    ( -- * Subtask W2.3.1
      Tree (..), relabelTree
      -- * Subtask W2.3.2
    , anything
      -- * Subtask W2.3.3
    , cast
    ) where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

-- Subatask W2.3.1

data Tree a = Leaf a | Node (Tree a) (Tree a)

relabelTree :: Tree a -> IO (Tree (a, Int))
relabelTree = error "TODO: implement relabelTree"

-- Subatask W2.3.2

-- |
-- >>> writeIORef anything True >> readIORef anything :: IO Bool
-- True
--
-- >>> writeIORef anything "Haskell" >> readIORef anything :: IO String
-- "Haskell"
--
anything :: IORef a
anything = error "TODO: implement anything"

-- Subatask W2.3.3

cast :: a -> b
cast = error "TODO: implement cast"

-- | W2.3 Unsafe IO
module W0203 where

import           Data.IORef
import           System.IO.Unsafe               ( )
-- ~\~ end
-- ~\~ begin <<docs/README.md|P03>>[1]
-- | Subatask 2.3.1
relabelTree :: Tree a -> IO (Tree (a, Int))
relabelTree = undefined

data Tree a = Leaf a | Node (Tree a) (Tree a)
-- ~\~ end
-- ~\~ begin <<docs/README.md|P03>>[2]
-- | Subtaks 2.3.2
anything :: IORef a
anything = undefined
-- ~\~ end
-- ~\~ begin <<docs/README.md|P03>>[3]
-- | Subtaks 2.3.3
cast :: a -> b
cast = undefined
-- ~\~ end

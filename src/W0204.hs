-- | W2.4 Transactions
module W0204
    ( -- * Types
      Output (..), Address, Input (..), Id, Index, Transaction (..), UTxOs
      -- * Subtask W2.4.1
    , processTransaction, processTransactions
      -- * Subtask W2.4.2
    , genesis, tx1, tx2, tx3, tx4, tx5
      -- * Subtask W2.4.3
    , ErrorState (..), throwError, get, put
      -- * Subtask W2.4.4
    , processTransaction', processTransactions'
    ) where

import Data.Map

data Output = Output
    { oValue   :: Int
    , oAddress :: Address
    } deriving (Show, Read, Eq, Ord)

type Address = String

data Input = Input
    { iPrevious :: Id
    , iIndex    :: Index
    } deriving (Show, Read, Eq, Ord)

type Id = Int
type Index = Int

data Transaction = Transaction
    { tId      :: Id
    , tInputs  :: [Input]
    , tOutputs :: [Output]
    } deriving (Show, Read, Eq, Ord)

type UTxOs = Map Input Output

-- Subtask W2.4.1

processTransaction :: Transaction -> UTxOs -> Either String UTxOs
processTransaction = error "TODO: implement processTransaction"

processTransactions :: [Transaction] -> UTxOs -> Either String UTxOs
processTransactions = error "TODO: implement processTransactions"

-- Subtask W2.4.2

-- | The initial @'UTxOs'@.
genesis :: UTxOs
genesis = error "TODO: implement genesis"

-- | A transaction which is invalid, because one of its inputs
-- does not exist.
--
-- >>> processTransaction tx1 genesis
-- Left ...
--
tx1 :: Transaction
tx1 = error "TODO: implement tx1"

-- | A transaction which is invalid, because the sum of input values
-- is smaller than the sum of output values.
--
-- >>> processTransaction tx2 genesis
-- Left ...
--
tx2 :: Transaction
tx2 = error "TODO: implement tx2"

-- | A valid transaction.
--
-- >>> processTransaction tx3 genesis
-- Right ...
--
tx3 :: Transaction
tx3 = error "TODO: implement tx3"

-- | Another valid transaction, but one that has an input
-- in common with @'tx3'@.
--
-- >>> processTransaction tx4 genesis
-- Right ...
--
-- >>> processTransactions [tx3, tx4] genesis
-- Left ...
--
tx4 :: Transaction
tx4 = error "TODO: implement tx4"

-- | A valid transaction, which can be combined with @'tx4'@.
--
-- >>> processTransaction tx5 genesis
-- Right ...
--
-- >>> processTransactions [tx4, tx5] genesis
-- Right ...
--
tx5 :: Transaction
tx5 = error "TODO: implement tx5"

-- Subtask W2.4.3

newtype ErrorState s a = ErrorState {runErrorState :: s -> Either String (a, s)}

instance Functor (ErrorState s) where
    fmap = error "TODO: implement fmap"

instance Applicative (ErrorState s) where
    pure = error "TODO: implement pure"
    (<*>) = error "TODO: implement (<*>)"

instance Monad (ErrorState s) where
    return = error "TODO: implement return"
    (>>=) = error "TODO: implement (>>=)"

throwError :: String -> ErrorState s a
throwError = error "TODO: implement throwError"

get :: ErrorState s s
get = error "TODO: implement get"

put :: s -> ErrorState s ()
put = error "TODO: implement put"

-- Subtask W2.4.4

-- |
-- >>> processTransaction' tx1 genesis
-- Left ...
--
-- >>> processTransaction' tx2 genesis
-- Left ...
--
-- >>> processTransaction' tx3 genesis
-- Right ...
--
-- >>> processTransaction' tx4 genesis
-- Right ...
--
-- >>> processTransaction' tx5 genesis
-- Right ...
--
processTransaction' :: Transaction -> UTxOs -> Either String UTxOs
processTransaction' = error "TODO: implement processTransaction'"

-- |
-- >>> processTransactions' [tx3, tx4] genesis
-- Left ...
--
-- >>> processTransactions' [tx4, tx5] genesis
-- Right ...
--
processTransactions' :: [Transaction] -> UTxOs -> Either String UTxOs
processTransactions' = error "TODO: implement processTransactions'"

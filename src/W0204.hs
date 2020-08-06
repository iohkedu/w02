-- | W2.4 Transactions
module W0204 where

import Data.Map

data Output = Output
    { oValue :: Int
    , oAddress :: Address
    } deriving (Show)

type Address = String

data Input = Input
    { iPrevious :: Id
    , iIndex    :: Index
    } deriving (Show, Eq, Ord)

type Index = Int

data Transaction = Transaction
    { tId :: Id
    , tInputs :: [Input]
    , tOutputs :: [Output]
    } deriving (Show)

type Id = Int

type UTXOs = Map Input Output

-- | Subtask 2.4.1
processTransactions :: [Transaction] -> UTXOs -> Either String UTXOs
processTransactions = undefined

-- | Subtask 2.4.2

-- | Subtask 2.4.3
--   Redefine UTXOs as
--  type UTXOs = Either String (a, UTXOs)
newtype ErrorState s a = ErrorState {runErrorState :: s -> Either String (a, s)}

throwError :: String -> ErrorState s a
throwError = undefined

get :: ErrorState s s
get = undefined

put :: s -> ErrorState s ()
put = undefined

-- | Subtask 2.4.4

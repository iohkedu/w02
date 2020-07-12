-- ~\~ language=Haskell filename=src/P05.hs
-- ~\~ begin <<docs/README.md|P05>>[0]
-- | W2.5 Transactions
module P05 where

  import Data.Map
  -- ~\~ begin <<docs/README.md|transaction_output_definition>>[0]
  data Output = Output { oValue :: Int
                       , oAddress :: Address
                       } deriving (Show)

  type Address = String
  -- ~\~ end
  -- ~\~ begin <<docs/README.md|transaction_input_definition>>[0]
  data Input = Input
   { iPrevious :: Id
   , iIndex :: Index
   }
   deriving (Show, Eq, Ord)

  type Index = Int
  -- ~\~ end

  data Transaction = Transaction { tId :: Id
                               , tInputs :: [Input]
                               , tOutputs :: [Output]
                               } deriving (Show)

  type Id = Int
-- ~\~ end
-- ~\~ begin <<docs/README.md|P05>>[1]
  type UTXOs = Map Input Output
-- ~\~ end
-- ~\~ begin <<docs/README.md|P05>>[2]
-- | Subtask 2.5.1
  processTransactions :: [Transaction] -> UTXOs -> Either String UTXOs
  processTransactions = undefined
-- ~\~ end
-- ~\~ begin <<docs/README.md|P05>>[3]
-- | Subtask 2.5.2

-- ~\~ end
-- ~\~ begin <<docs/README.md|P05>>[4]
-- | Subtask 2.5.3
--   Redefine UTXOs as
--  type UTXOs = Either String (a, UTXOs)
-- ~\~ end
-- ~\~ begin <<docs/README.md|P05>>[5]
  newtype ErrorState s a = ErrorState {runErrorState :: s -> Either String (a, s)}

-- ~\~ end
-- ~\~ begin <<docs/README.md|P05>>[6]

  throwError :: String -> ErrorState s a
  throwError = undefined

  get :: ErrorState s s
  get = undefined

  put :: s -> ErrorState s ()
  put = undefined
-- ~\~ end
-- ~\~ begin <<docs/README.md|P05>>[7]
-- | Subtask 2.5.4

-- ~\~ end

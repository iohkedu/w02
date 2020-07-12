-- ~\~ language=Haskell filename=test/P05Spec.hs
-- ~\~ begin <<docs/README.md|examples>>[0]
module P05Spec where

import           P05
import qualified Data.Map                      as M

-- ~\~ end
-- ~\~ begin <<docs/README.md|examples>>[1]
genesis :: UTXOs
genesis = M.fromList [(Input 0 (-1), Output 10000 "John")]

-- ~\~ end
-- ~\~ begin <<docs/README.md|examples>>[2]
perDiem :: Transaction
perDiem = Transaction
  { tId      = 1
  , tInputs  = [Input 0 (-1)]
  , tOutputs = [ Output 1500 "Lillian"
               , Output 1500 "Meron"
               , Output 1500 "Melikte"
               , Output 5500 "John"
               ]
  }
-- ~\~ end
-- ~\~ begin <<docs/README.md|examples>>[3]
spec :: Spec
spec = do
  describe "process UTXos" $ do
    context "John pay's 1500 to Lillian, Meron and Melikte" $ do
      it "is valid trancation and spent input has been removed" $ do
        let Right utxos1 = processTransaction perDiem genesis
        utxos `shouldBe` M.fromList
          [ (Input 1 0, Output 1500 "Lillian")
          , (Inout 1 1, Output 1500 "Meron")
          , (Input 1 2, Output 1500 "Melikte")
          , (Input 1 3, Output 5500 "John")
          ]
     -- ~\~ begin <<docs/README.md|next_transaction>>[0]
      context "Meron and Melikte want to share lunch together" $ do
        it "is valid, and two inputs have been removed" $ do
          let Right utxos2 = processTransaction lunch utxos
          utxos `shouldBe` M.fromList
            [ (Input 1 0, Output 1500 "Lillian")
            , (Input 1 3, Output 5500 "John")
            , (Input 2 0, Output 100 "Bethel's Fine Ethiopian Food")
            , (Input 2 1, Output 1440 "Meron")
            , (Input 2 2, Output 1440 "Melikte")
            ]

     -- ~\~ end
-- ~\~ end
-- ~\~ begin <<docs/README.md|examples>>[4]
lunch :: Transaction
lunch = Transaction
  { tId      = 2
  , tInputs  = [Input 1 1, Input 1 2]
  , tOutputs = [ Output 100  "Bethel's Fine Ethiopian Food"
               , Output 1440 "Meron"
               , Output 1440 "Melikte"
               ]
  }
-- ~\~ end

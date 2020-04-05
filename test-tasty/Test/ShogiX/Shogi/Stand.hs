module Test.ShogiX.Shogi.Stand where

import           RIO
import qualified RIO.Map                       as Map
import qualified RIO.Set                       as Set
import           Test.Tasty
import           Test.Tasty.Hspec
import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Stand            as Stand

{-# ANN module "HLint: ignore Use camelCase" #-}
spec_droppables :: Spec
spec_droppables = describe "droppables" $ do
  describe "将棋盤も駒台も空の場合"
    $ it "空を返す"
    $ Stand.droppables Black (Board Map.empty) (Stands Stand.empty Stand.empty)
    `shouldBe` Droppables Map.empty
  describe "打ち込める駒がある場合"
    $          it "駒ごとの打ち込み先を返す"
    $          Stand.droppables
                 Black
                 (Board Map.empty)
                 (Stands (Stand (Map.fromList [(Pawn, 1), (Gold, 1)])) Stand.empty)
    `shouldBe` Droppables
                 (Map.fromList
                   [ ( Pawn
                     , Droppable
                       (Set.fromList
                         [ (file, rank)
                         | file <- [F9 .. F1]
                         , rank <- [R2 .. R9]
                         ]
                       )
                     )
                   , ( Gold
                     , Droppable
                       (Set.fromList
                         [ (file, rank)
                         | file <- [F9 .. F1]
                         , rank <- [R1 .. R9]
                         ]
                       )
                     )
                   ]
                 )

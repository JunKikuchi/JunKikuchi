module Test.ShogiX.Shogi.Stand where

import           RIO
import qualified RIO.Map                       as Map
import           Test.Tasty
import           Test.Tasty.Hspec
import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Board            as Board
import qualified ShogiX.Shogi.Stands           as Stands
import qualified ShogiX.Shogi.Stand            as Stand
import qualified ShogiX.Shogi.Droppables       as Droppables

{-# ANN module "HLint: ignore Use camelCase" #-}
spec_Test_ShogiX_Shogi_Stand :: Spec
spec_Test_ShogiX_Shogi_Stand = describe "droppables" $ do
  describe "将棋盤も駒台も空の場合"
    $          it "空を返す"
    $          Stand.droppables Black (Board Map.empty) Stands.empty
    `shouldBe` Droppables Map.empty
  describe "打ち込める駒がある場合"
    $          it "駒ごとの打ち込み先を返す"
    $          Stand.droppables Black
                                (Board Map.empty)
                                (Stands.fromList [(Pawn, 1), (Gold, 1)] [])
    `shouldBe` Droppables.fromList
                 [ ( Pawn
                   , [ (file, rank) | file <- [F9 .. F1], rank <- [R2 .. R9] ]
                   )
                 , ( Gold
                   , [ (file, rank) | file <- [F9 .. F1], rank <- [R1 .. R9] ]
                   )
                 ]
  describe "王手されている場合" $ do
    describe "先手"
      $          it "王手回避する打ち込み先を返さない"
      $          Stand.droppables
                   Black
                   (Board.fromList
                     [((F5, R9), Piece Black King), ((F5, R5), Piece White Lance)]
                   )
                   (Stands.fromList [(Pawn, 1)] [])
      `shouldBe` Droppables.fromList
                   [ ( Pawn
                     , [ (file, rank)
                       | file <- [F9 .. F1]
                       , rank <- [R2 .. R9]
                       , (F5, R9) /= (file, rank) && (F5, R5) /= (file, rank)
                       ]
                     )
                   ]
    describe "後手"
      $          it "王手回避する打ち込み先を返さない"
      $          Stand.droppables
                   White
                   (Board.fromList
                     [((F5, R1), Piece White King), ((F5, R5), Piece Black Lance)]
                   )
                   (Stands.fromList [] [(Pawn, 1)])
      `shouldBe` Droppables.fromList
                   [ ( Pawn
                     , [ (file, rank)
                       | file <- [F9 .. F1]
                       , rank <- [R1 .. R8]
                       , (F5, R1) /= (file, rank) && (F5, R5) /= (file, rank)
                       ]
                     )
                   ]

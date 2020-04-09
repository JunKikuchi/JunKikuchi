module Test.ShogiX.Shogi.Board where
import           RIO
import qualified RIO.Map                       as Map
import qualified RIO.Set                       as Set
import           Test.Tasty
import           Test.Tasty.Hspec
import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Board            as Board

{-# ANN module "HLint: ignore Use camelCase" #-}
spec_check :: Spec
spec_check = describe "check" $ do
  describe "王手されていない場合" $ do
    describe "先手"
      $          it "王手判定 False"
      $          Board.checked
                   Black
                   (Board
                     (Map.fromList
                       [((F5, R9), Piece Black King), ((F5, R1), Piece White Pawn)]
                     )
                   )
      `shouldBe` False
    describe "後手"
      $          it "王手判定 False"
      $          Board.checked
                   White
                   (Board
                     (Map.fromList
                       [((F5, R1), Piece White King), ((F5, R9), Piece Black Pawn)]
                     )
                   )
      `shouldBe` False
  describe "王手されている場合" $ do
    describe "先手"
      $          it "王手判定 True"
      $          Board.checked
                   Black
                   (Board
                     (Map.fromList
                       [((F5, R9), Piece Black King), ((F5, R8), Piece White Pawn)]
                     )
                   )
      `shouldBe` True
    describe "後手"
      $          it "王手判定 True"
      $          Board.checked
                   White
                   (Board
                     (Map.fromList
                       [((F5, R1), Piece White King), ((F5, R2), Piece Black Pawn)]
                     )
                   )
      `shouldBe` True

spec_movables :: Spec
spec_movables = describe "movables" $ do
  describe "将棋盤が空の場合"
    $          it "空を返す"
    $          Board.movables Black (Board Map.empty)
    `shouldBe` Movables Map.empty
  describe "将棋盤に駒がある場合"
    $          it "駒ごとの可動範囲を返す"
    $          Board.movables
                 Black
                 (Board
                   (Map.fromList
                     [ ((F5, R5), Piece Black Pawn)
                     , ((F5, R9), Piece Black Gold)
                     , ((F5, R1), Piece White Pawn)
                     ]
                   )
                 )
    `shouldBe` Movables
                 (Map.fromList
                   [ ((F5, R5), Movable (Map.fromList [((F5, R4), No)]))
                   , ( (F5, R9)
                     , Movable
                       (Map.fromList
                         [ ((F6, R8), No)
                         , ((F5, R8), No)
                         , ((F4, R8), No)
                         , ((F4, R9), No)
                         , ((F6, R9), No)
                         ]
                       )
                     )
                   ]
                 )
  describe "王手されている場合" $ do
    describe "先手"
      $          it "王手回避する可動範囲を返さない"
      $          Board.movables
                   Black
                   (Board
                     (Map.fromList
                       [ ((F5, R9), Piece Black King)
                       , ((F5, R5), Piece Black Pawn)
                       , ((F5, R8), Piece White Gold)
                       ]
                     )
                   )
      `shouldBe` Movables
                   (Map.fromList
                     [ ( (F5, R9)
                       , Movable
                         (Map.fromList
                           [ ((F6, R8), No)
                           , ((F5, R8), No)
                           , ((F4, R8), No)
                           , ((F4, R9), No)
                           , ((F6, R9), No)
                           ]
                         )
                       )
                     , ((F5, R5), Movable (Map.fromList [((F5, R4), No)]))
                     ]
                   )
    describe "後手"
      $          it "王手回避する可動範囲を返さない"
      $          Board.movables
                   White
                   (Board
                     (Map.fromList
                       [ ((F5, R1), Piece White King)
                       , ((F5, R5), Piece White Pawn)
                       , ((F5, R2), Piece Black Gold)
                       ]
                     )
                   )
      `shouldBe` Movables
                   (Map.fromList
                     [ ( (F5, R1)
                       , Movable
                         (Map.fromList
                           [ ((F6, R2), No)
                           , ((F5, R2), No)
                           , ((F4, R2), No)
                           , ((F6, R1), No)
                           , ((F4, R1), No)
                           ]
                         )
                       )
                     , ((F5, R5), Movable (Map.fromList [((F5, R6), No)]))
                     ]
                   )

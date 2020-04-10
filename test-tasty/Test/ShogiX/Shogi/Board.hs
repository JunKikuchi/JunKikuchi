module Test.ShogiX.Shogi.Board where
import           RIO
import qualified RIO.Map                       as Map
import qualified RIO.Set                       as Set
import           Test.Tasty
import           Test.Tasty.Hspec
import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Board            as Board

{-# ANN module "HLint: ignore Use camelCase" #-}
spec_move :: Spec
spec_move = describe "move" $ do
  describe "移動元あり" $ do
    describe "移動先が可動範囲内" $ do
      describe "駒" $ do
        describe "不成" $ do
          describe "そのまま" $ do
            describe "移動先が空" $ do
              describe "先手"
                $          it "駒を移動する"
                $          Board.move
                             (F5, R5)
                             False
                             (F5, R4)
                             (Board (Map.fromList [((F5, R5), Piece Black Pawn)]))
                `shouldBe` Just
                             ( Board
                               (Map.fromList [((F5, R4), Piece Black Pawn)])
                             , Nothing
                             )
              describe "後手"
                $          it "駒を移動する"
                $          Board.move
                             (F5, R5)
                             False
                             (F5, R6)
                             (Board (Map.fromList [((F5, R5), Piece White Pawn)]))
                `shouldBe` Just
                             ( Board
                               (Map.fromList [((F5, R6), Piece White Pawn)])
                             , Nothing
                             )
            describe "移動先に相手の駒がある" $ do
              describe "先手"
                $          it "相手の駒をとって駒を移動する"
                $          Board.move
                             (F5, R5)
                             False
                             (F5, R4)
                             (Board
                               (Map.fromList
                                 [ ((F5, R5), Piece Black Pawn)
                                 , ((F5, R4), Piece White Gold)
                                 ]
                               )
                             )
                `shouldBe` Just
                             ( Board
                               (Map.fromList [((F5, R4), Piece Black Pawn)])
                             , Just Gold
                             )
              describe "後手"
                $          it "相手の駒をとって駒を移動する"
                $          Board.move
                             (F5, R5)
                             False
                             (F5, R6)
                             (Board
                               (Map.fromList
                                 [ ((F5, R5), Piece White Pawn)
                                 , ((F5, R6), Piece Black Gold)
                                 ]
                               )
                             )
                `shouldBe` Just
                             ( Board
                               (Map.fromList [((F5, R6), Piece White Pawn)])
                             , Just Gold
                             )
            describe "移動先に味方の駒がある" $ do
              describe "先手"
                $          it "Nothing"
                $          Board.move
                             (F5, R5)
                             False
                             (F5, R4)
                             (Board
                               (Map.fromList
                                 [ ((F5, R5), Piece Black Pawn)
                                 , ((F5, R4), Piece Black Gold)
                                 ]
                               )
                             )
                `shouldBe` Nothing
              describe "後手"
                $          it "Nothing"
                $          Board.move
                             (F5, R5)
                             False
                             (F5, R6)
                             (Board
                               (Map.fromList
                                 [ ((F5, R5), Piece White Pawn)
                                 , ((F5, R6), Piece White Gold)
                                 ]
                               )
                             )
                `shouldBe` Nothing
          describe "成り必須" $ do
            describe "先手"
              $          it "Nothing"
              $          Board.move
                           (F5, R2)
                           False
                           (F5, R1)
                           (Board (Map.fromList [((F5, R2), Piece Black Pawn)]))
              `shouldBe` Nothing
            describe "後手"
              $          it "Nothing"
              $          Board.move
                           (F5, R8)
                           False
                           (F5, R9)
                           (Board (Map.fromList [((F5, R8), Piece White Pawn)]))
              `shouldBe` Nothing
        describe "成り" $ do
          describe "成り可" $ do
            describe "先手"
              $          it "駒を移動する"
              $          Board.move
                           (F5, R4)
                           True
                           (F5, R3)
                           (Board (Map.fromList [((F5, R4), Piece Black Pawn)]))
              `shouldBe` Just
                           ( Board
                             (Map.fromList
                               [((F5, R3), Piece Black PromotedPawn)]
                             )
                           , Nothing
                           )
            describe "後手"
              $          it "駒を移動する"
              $          Board.move
                           (F5, R6)
                           True
                           (F5, R7)
                           (Board (Map.fromList [((F5, R6), Piece White Pawn)]))
              `shouldBe` Just
                           ( Board
                             (Map.fromList
                               [((F5, R7), Piece White PromotedPawn)]
                             )
                           , Nothing
                           )
          describe "成り不可" $ do
            describe "先手"
              $          it "Nothing"
              $          Board.move
                           (F5, R5)
                           True
                           (F5, R4)
                           (Board (Map.fromList [((F5, R5), Piece Black Pawn)]))
              `shouldBe` Nothing
            describe "後手"
              $          it "Nothing"
              $          Board.move
                           (F5, R5)
                           True
                           (F5, R6)
                           (Board (Map.fromList [((F5, R5), Piece White Pawn)]))
              `shouldBe` Nothing
      describe "成り駒" $ do
        describe "そのまま" $ do
          describe "先手"
            $          it "駒を移動する"
            $          Board.move
                         (F5, R4)
                         False
                         (F5, R3)
                         (Board (Map.fromList [((F5, R4), Piece Black PromotedPawn)]))
            `shouldBe` Just
                         ( Board
                           (Map.fromList [((F5, R3), Piece Black PromotedPawn)])
                         , Nothing
                         )
          describe "後手"
            $          it "駒を移動する"
            $          Board.move
                         (F5, R7)
                         False
                         (F5, R8)
                         (Board (Map.fromList [((F5, R7), Piece White PromotedPawn)]))
            `shouldBe` Just
                         ( Board
                           (Map.fromList [((F5, R8), Piece White PromotedPawn)])
                         , Nothing
                         )
        describe "成り不可" $ do
          describe "先手"
            $          it "Nothing"
            $          Board.move
                         (F5, R4)
                         True
                         (F5, R3)
                         (Board (Map.fromList [((F5, R4), Piece Black PromotedPawn)]))
            `shouldBe` Nothing
          describe "後手"
            $          it "Nothing"
            $          Board.move
                         (F5, R7)
                         True
                         (F5, R8)
                         (Board (Map.fromList [((F5, R7), Piece White PromotedPawn)]))
            `shouldBe` Nothing
    describe "移動先が可動範囲外" $ do
      describe "先手"
        $ it "Nothing"
        $ Board.move (F5, R5)
                     False
                     (F5, R3)
                     (Board (Map.fromList [((F5, R5), Piece Black Pawn)]))
        `shouldBe` Nothing
      describe "後手"
        $ it "Nothing"
        $ Board.move (F5, R5)
                     False
                     (F5, R7)
                     (Board (Map.fromList [((F5, R5), Piece White Pawn)]))
        `shouldBe` Nothing
  describe "移動元なし"
    $          it "Nothing"
    $          Board.move (F5, R5) False (F5, R4) (Board Map.empty)
    `shouldBe` Nothing

spec_checked :: Spec
spec_checked = describe "checked" $ do
  describe "玉将がない場合" $ do
    describe "先手"
      $          it "王手判定 False"
      $          Board.checked
                   Black
                   (Board
                     (Map.fromList
                       [((F5, R9), Piece Black Pawn), ((F5, R1), Piece White Pawn)]
                     )
                   )
      `shouldBe` False
    describe "後手"
      $          it "王手判定 False"
      $          Board.checked
                   White
                   (Board
                     (Map.fromList
                       [((F5, R1), Piece White Pawn), ((F5, R9), Piece Black Pawn)]
                     )
                   )
      `shouldBe` False
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

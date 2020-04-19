module Test.ShogiX.Shogi.Board where
import           RIO
import qualified RIO.Map                       as Map
import qualified RIO.Set                       as Set
import           Test.Tasty
import           Test.Tasty.Hspec
import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Board            as Board
import qualified ShogiX.Shogi.Movables         as Movables

{-# ANN module "HLint: ignore Use camelCase" #-}
spec_Test_ShogiX_Shogi_Board :: Spec
spec_Test_ShogiX_Shogi_Board = do
  describe "move" $ do
    describe "移動元あり" $ do
      describe "移動先が可動範囲内" $ do
        describe "駒" $ do
          describe "不成" $ do
            describe "そのまま" $ do
              describe "移動先が空" $ do
                describe "先手"
                  $ it "駒を移動する"
                  $ Board.move (F5, R5)
                               False
                               (F5, R4)
                               (Board.fromList [((F5, R5), Piece Black Pawn)])
                  `shouldBe` Just
                               ( Board.fromList [((F5, R4), Piece Black Pawn)]
                               , Nothing
                               )
                describe "後手"
                  $ it "駒を移動する"
                  $ Board.move (F5, R5)
                               False
                               (F5, R6)
                               (Board.fromList [((F5, R5), Piece White Pawn)])
                  `shouldBe` Just
                               ( Board.fromList [((F5, R6), Piece White Pawn)]
                               , Nothing
                               )
              describe "移動先に相手の駒がある" $ do
                describe "先手"
                  $          it "相手の駒をとって駒を移動する"
                  $          Board.move
                               (F5, R5)
                               False
                               (F5, R4)
                               (Board.fromList
                                 [ ((F5, R5), Piece Black Pawn)
                                 , ((F5, R4), Piece White Gold)
                                 ]
                               )
                  `shouldBe` Just
                               ( Board.fromList [((F5, R4), Piece Black Pawn)]
                               , Just Gold
                               )
                describe "後手"
                  $          it "相手の駒をとって駒を移動する"
                  $          Board.move
                               (F5, R5)
                               False
                               (F5, R6)
                               (Board.fromList
                                 [ ((F5, R5), Piece White Pawn)
                                 , ((F5, R6), Piece Black Gold)
                                 ]
                               )
                  `shouldBe` Just
                               ( Board.fromList [((F5, R6), Piece White Pawn)]
                               , Just Gold
                               )
              describe "移動先に味方の駒がある" $ do
                describe "先手"
                  $          it "Nothing"
                  $          Board.move
                               (F5, R5)
                               False
                               (F5, R4)
                               (Board.fromList
                                 [ ((F5, R5), Piece Black Pawn)
                                 , ((F5, R4), Piece Black Gold)
                                 ]
                               )
                  `shouldBe` Nothing
                describe "後手"
                  $          it "Nothing"
                  $          Board.move
                               (F5, R5)
                               False
                               (F5, R6)
                               (Board.fromList
                                 [ ((F5, R5), Piece White Pawn)
                                 , ((F5, R6), Piece White Gold)
                                 ]
                               )
                  `shouldBe` Nothing
            describe "成り必須" $ do
              describe "先手"
                $ it "Nothing"
                $ Board.move (F5, R2)
                             False
                             (F5, R1)
                             (Board.fromList [((F5, R2), Piece Black Pawn)])
                `shouldBe` Nothing
              describe "後手"
                $ it "Nothing"
                $ Board.move (F5, R8)
                             False
                             (F5, R9)
                             (Board.fromList [((F5, R8), Piece White Pawn)])
                `shouldBe` Nothing
          describe "成り" $ do
            describe "成り可" $ do
              describe "先手"
                $ it "駒を移動する"
                $ Board.move (F5, R4)
                             True
                             (F5, R3)
                             (Board.fromList [((F5, R4), Piece Black Pawn)])
                `shouldBe` Just
                             ( Board
                               (Map.fromList
                                 [((F5, R3), Piece Black PromotedPawn)]
                               )
                             , Nothing
                             )
              describe "後手"
                $ it "駒を移動する"
                $ Board.move (F5, R6)
                             True
                             (F5, R7)
                             (Board.fromList [((F5, R6), Piece White Pawn)])
                `shouldBe` Just
                             ( Board
                               (Map.fromList
                                 [((F5, R7), Piece White PromotedPawn)]
                               )
                             , Nothing
                             )
            describe "成り不可" $ do
              describe "先手"
                $ it "Nothing"
                $ Board.move (F5, R5)
                             True
                             (F5, R4)
                             (Board.fromList [((F5, R5), Piece Black Pawn)])
                `shouldBe` Nothing
              describe "後手"
                $ it "Nothing"
                $ Board.move (F5, R5)
                             True
                             (F5, R6)
                             (Board.fromList [((F5, R5), Piece White Pawn)])
                `shouldBe` Nothing
        describe "成り駒" $ do
          describe "そのまま" $ do
            describe "先手"
              $          it "駒を移動する"
              $          Board.move
                           (F5, R4)
                           False
                           (F5, R3)
                           (Board.fromList [((F5, R4), Piece Black PromotedPawn)])
              `shouldBe` Just
                           ( Board.fromList
                             [((F5, R3), Piece Black PromotedPawn)]
                           , Nothing
                           )
            describe "後手"
              $          it "駒を移動する"
              $          Board.move
                           (F5, R7)
                           False
                           (F5, R8)
                           (Board.fromList [((F5, R7), Piece White PromotedPawn)])
              `shouldBe` Just
                           ( Board.fromList
                             [((F5, R8), Piece White PromotedPawn)]
                           , Nothing
                           )
          describe "成り不可" $ do
            describe "先手"
              $          it "Nothing"
              $          Board.move
                           (F5, R4)
                           True
                           (F5, R3)
                           (Board.fromList [((F5, R4), Piece Black PromotedPawn)])
              `shouldBe` Nothing
            describe "後手"
              $          it "Nothing"
              $          Board.move
                           (F5, R7)
                           True
                           (F5, R8)
                           (Board.fromList [((F5, R7), Piece White PromotedPawn)])
              `shouldBe` Nothing
        describe "王手されている場合" $ do
          describe "先手"
            $          it "移動後の盤面を返す"
            $          Board.move
                         (F4, R5)
                         False
                         (F4, R4)
                         (Board.fromList
                           [ ((F5, R9), Piece Black King)
                           , ((F5, R1), Piece White Lance)
                           , ((F4, R5), Piece Black Pawn)
                           ]
                         )
            `shouldBe` Just
                         ( Board.fromList
                           [ ((F5, R9), Piece Black King)
                           , ((F5, R1), Piece White Lance)
                           , ((F4, R4), Piece Black Pawn)
                           ]
                         , Nothing
                         )
          describe "後手"
            $          it "移動後の盤面を返す"
            $          Board.move
                         (F6, R5)
                         False
                         (F6, R6)
                         (Board.fromList
                           [ ((F5, R1), Piece White King)
                           , ((F5, R9), Piece Black Lance)
                           , ((F6, R5), Piece White Pawn)
                           ]
                         )
            `shouldBe` Just
                         ( Board.fromList
                           [ ((F5, R1), Piece White King)
                           , ((F5, R9), Piece Black Lance)
                           , ((F6, R6), Piece White Pawn)
                           ]
                         , Nothing
                         )
        describe "移動すると王手になる場合" $ do
          describe "先手"
            $          it "移動後の盤面を返す"
            $          Board.move
                         (F5, R5)
                         False
                         (F4, R5)
                         (Board.fromList
                           [ ((F5, R9), Piece Black King)
                           , ((F5, R1), Piece White Lance)
                           , ((F5, R5), Piece Black Gold)
                           ]
                         )
            `shouldBe` Just
                         ( Board.fromList
                           [ ((F5, R9), Piece Black King)
                           , ((F5, R1), Piece White Lance)
                           , ((F4, R5), Piece Black Gold)
                           ]
                         , Nothing
                         )
          describe "後手"
            $          it "移動後の盤面を返す"
            $          Board.move
                         (F5, R5)
                         False
                         (F6, R5)
                         (Board.fromList
                           [ ((F5, R1), Piece White King)
                           , ((F5, R9), Piece Black Lance)
                           , ((F5, R5), Piece White Gold)
                           ]
                         )
            `shouldBe` Just
                         ( Board.fromList
                           [ ((F5, R1), Piece White King)
                           , ((F5, R9), Piece Black Lance)
                           , ((F6, R5), Piece White Gold)
                           ]
                         , Nothing
                         )
      describe "移動先が可動範囲外" $ do
        describe "先手"
          $          it "Nothing"
          $          Board.move (F5, R5)
                                False
                                (F5, R3)
                                (Board.fromList [((F5, R5), Piece Black Pawn)])
          `shouldBe` Nothing
        describe "後手"
          $          it "Nothing"
          $          Board.move (F5, R5)
                                False
                                (F5, R7)
                                (Board.fromList [((F5, R5), Piece White Pawn)])
          `shouldBe` Nothing
    describe "移動元なし"
      $          it "Nothinge"
      $          Board.move (F5, R5) False (F5, R4) (Board Map.empty)
      `shouldBe` Nothing

  describe "drop" $ do
    describe "打ち込める場合" $ do
      describe "先手"
        $          it "駒を打ち込む"
        $          Board.drop Black Pawn (F5, R5) (Board.fromList [])
        `shouldBe` Just (Board.fromList [((F5, R5), Piece Black Pawn)])
      describe "後手"
        $          it "駒を打ち込む"
        $          Board.drop White Pawn (F5, R5) (Board.fromList [])
        `shouldBe` Just (Board.fromList [((F5, R5), Piece White Pawn)])
      describe "王手されている場合" $ do
        describe "先手"
          $          it "駒を打ち込む"
          $          Board.drop
                       Black
                       Pawn
                       (F4, R5)
                       (Board.fromList
                         [((F5, R9), Piece Black King), ((F5, R1), Piece White Lance)]
                       )
          `shouldBe` Just
                       (Board.fromList
                         [ ((F5, R9), Piece Black King)
                         , ((F5, R1), Piece White Lance)
                         , ((F4, R5), Piece Black Pawn)
                         ]
                       )
        describe "後手"
          $          it "駒を打ち込む"
          $          Board.drop
                       White
                       Pawn
                       (F6, R5)
                       (Board.fromList
                         [((F5, R1), Piece White King), ((F5, R9), Piece Black Lance)]
                       )
          `shouldBe` Just
                       (Board.fromList
                         [ ((F5, R1), Piece White King)
                         , ((F5, R9), Piece Black Lance)
                         , ((F6, R5), Piece White Pawn)
                         ]
                       )
    describe "打ち込めない場合" $ do
      describe "成り駒を打ち込んだ場合" $ do
        describe "先手"
          $          it "Nothing"
          $          Board.drop Black PromotedPawn (F5, R5) (Board.fromList [])
          `shouldBe` Nothing
        describe "後手"
          $          it "Nothing"
          $          Board.drop White PromotedPawn (F5, R5) (Board.fromList [])
          `shouldBe` Nothing
      describe "打ち込める範囲外の場合" $ do
        describe "先手"
          $          it "Nothing"
          $          Board.drop Black PromotedPawn (F5, R1) (Board.fromList [])
          `shouldBe` Nothing
        describe "後手"
          $          it "Nothing"
          $          Board.drop White PromotedPawn (F5, R9) (Board.fromList [])
          `shouldBe` Nothing
      describe "打ち込み先に駒がある場合" $ do
        describe "先手" $ do
          it "Nothing"
            $ Board.drop Black
                         Pawn
                         (F5, R5)
                         (Board.fromList [((F5, R5), Piece Black Pawn)])
            `shouldBe` Nothing
          it "Nothing"
            $ Board.drop Black
                         Pawn
                         (F5, R5)
                         (Board.fromList [((F5, R5), Piece White Pawn)])
            `shouldBe` Nothing
        describe "後手" $ do
          it "Nothing"
            $ Board.drop White
                         Pawn
                         (F5, R5)
                         (Board.fromList [((F5, R5), Piece White Pawn)])
            `shouldBe` Nothing
          it "Nothing"
            $ Board.drop White
                         PromotedPawn
                         (F5, R5)
                         (Board.fromList [((F5, R5), Piece Black Pawn)])
            `shouldBe` Nothing

  describe "checked" $ do
    describe "玉将がない場合" $ do
      describe "先手"
        $          it "王手判定 False"
        $          Board.checked
                     Black
                     (Board.fromList
                       [((F5, R9), Piece Black Pawn), ((F5, R1), Piece White Pawn)]
                     )
        `shouldBe` False
      describe "後手"
        $          it "王手判定 False"
        $          Board.checked
                     White
                     (Board.fromList
                       [((F5, R1), Piece White Pawn), ((F5, R9), Piece Black Pawn)]
                     )
        `shouldBe` False
    describe "王手されていない場合" $ do
      describe "先手"
        $          it "王手判定 False"
        $          Board.checked
                     Black
                     (Board.fromList
                       [((F5, R9), Piece Black King), ((F5, R1), Piece White Pawn)]
                     )
        `shouldBe` False
      describe "後手"
        $          it "王手判定 False"
        $          Board.checked
                     White
                     (Board.fromList
                       [((F5, R1), Piece White King), ((F5, R9), Piece Black Pawn)]
                     )
        `shouldBe` False
    describe "王手されている場合" $ do
      describe "先手"
        $          it "王手判定 True"
        $          Board.checked
                     Black
                     (Board.fromList
                       [((F5, R9), Piece Black King), ((F5, R8), Piece White Pawn)]
                     )
        `shouldBe` True
      describe "後手"
        $          it "王手判定 True"
        $          Board.checked
                     White
                     (Board.fromList
                       [((F5, R1), Piece White King), ((F5, R2), Piece Black Pawn)]
                     )
        `shouldBe` True

  describe "movables" $ do
    describe "将棋盤が空の場合"
      $          it "空を返す"
      $          Board.movables Black (Board Map.empty)
      `shouldBe` Movables Map.empty
    describe "将棋盤に駒がある場合"
      $          it "駒ごとの可動範囲を返す"
      $          Board.movables
                   Black
                   (Board.fromList
                     [ ((F5, R5), Piece Black Pawn)
                     , ((F5, R9), Piece Black Gold)
                     , ((F5, R1), Piece White Pawn)
                     ]
                   )
      `shouldBe` Movables.fromList
                   [ ((F5, R5), [((F5, R4), No)])
                   , ( (F5, R9)
                     , [ ((F6, R8), No)
                       , ((F5, R8), No)
                       , ((F4, R8), No)
                       , ((F4, R9), No)
                       , ((F6, R9), No)
                       ]
                     )
                   ]
    describe "王手されている場合" $ do
      describe "先手"
        $          it "王手回避する可動範囲を返さない"
        $          Board.movables
                     Black
                     (Board.fromList
                       [ ((F5, R9), Piece Black King)
                       , ((F5, R5), Piece Black Pawn)
                       , ((F5, R8), Piece White Gold)
                       ]
                     )
        `shouldBe` Movables.fromList
                     [ ( (F5, R9)
                       , [ ((F6, R8), No)
                         , ((F5, R8), No)
                         , ((F4, R8), No)
                         , ((F4, R9), No)
                         , ((F6, R9), No)
                         ]
                       )
                     , ((F5, R5), [((F5, R4), No)])
                     ]
      describe "後手"
        $          it "王手回避する可動範囲を返さない"
        $          Board.movables
                     White
                     (Board.fromList
                       [ ((F5, R1), Piece White King)
                       , ((F5, R5), Piece White Pawn)
                       , ((F5, R2), Piece Black Gold)
                       ]
                     )
        `shouldBe` Movables.fromList
                     [ ( (F5, R1)
                       , [ ((F6, R2), No)
                         , ((F5, R2), No)
                         , ((F4, R2), No)
                         , ((F6, R1), No)
                         , ((F4, R1), No)
                         ]
                       )
                     , ((F5, R5), [((F5, R6), No)])
                     ]

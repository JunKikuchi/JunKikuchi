module Test.ShogiX.Shogi.Position where

import           RIO
import           Test.Tasty
import           Test.Tasty.Hspec
import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Position         as Position
import qualified ShogiX.Shogi.Board            as Board
import qualified ShogiX.Shogi.Stands           as Stands
import qualified ShogiX.Shogi.Movables         as Movables
import qualified ShogiX.Shogi.Droppables       as Droppables
import           ShogiX.Clocks                 as Clocks

{-# ANN module "HLint: ignore Use camelCase" #-}
spec_Test_ShogiX_Shogi_Position :: Spec
spec_Test_ShogiX_Shogi_Position = do
  describe "move" $ do
    describe "駒の移動ができる場合" $ do
      describe "先手"
        $          it "移動後の局面を返す"
        $          Position.move
                     (F5, R5)
                     False
                     (F5, R4)
                     3
                     (Position Black
                               (Board.fromList [((F5, R5), Piece Black Pawn)])
                               Stands.empty
                               (Clocks.guillotine 10)
                     )
        `shouldBe` Just
                     (Position White
                               (Board.fromList [((F5, R4), Piece Black Pawn)])
                               Stands.empty
                               (Clocks.Clocks (Guillotine 7) (Guillotine 10))
                     )
      describe "後手"
        $          it "移動後の局面を返す"
        $          Position.move
                     (F5, R5)
                     False
                     (F5, R6)
                     3
                     (Position White
                               (Board.fromList [((F5, R5), Piece White Pawn)])
                               Stands.empty
                               (Clocks.guillotine 10)
                     )
        `shouldBe` Just
                     (Position Black
                               (Board.fromList [((F5, R6), Piece White Pawn)])
                               Stands.empty
                               (Clocks.Clocks (Guillotine 10) (Guillotine 7))
                     )
      describe "駒を取った場合" $ do
        describe "先手"
          $          it "移動後の局面を返す"
          $          Position.move
                       (F5, R5)
                       False
                       (F5, R4)
                       3
                       (Position
                         Black
                         (Board.fromList
                           [((F5, R5), Piece Black Pawn), ((F5, R4), Piece White Pawn)]
                         )
                         Stands.empty
                         Clocks.infinity
                       )
          `shouldBe` Just
                       (Position
                         White
                         (Board.fromList [((F5, R4), Piece Black Pawn)])
                         (Stands.fromList [(Pawn, 1)] [])
                         Clocks.infinity
                       )
        describe "後手"
          $          it "移動後の局面を返す"
          $          Position.move
                       (F5, R5)
                       False
                       (F5, R6)
                       3
                       (Position
                         White
                         (Board.fromList
                           [((F5, R5), Piece White Pawn), ((F5, R6), Piece Black Pawn)]
                         )
                         Stands.empty
                         Clocks.infinity
                       )
          `shouldBe` Just
                       (Position
                         Black
                         (Board.fromList [((F5, R6), Piece White Pawn)])
                         (Stands.fromList [] [(Pawn, 1)])
                         Clocks.infinity
                       )
    describe "駒の移動が出来ない場合" $ do
      describe "先手"
        $          it "Nothing"
        $          Position.move
                     (F5, R5)
                     False
                     (F5, R3)
                     3
                     (Position Black
                               (Board.fromList [((F5, R5), Piece Black Pawn)])
                               Stands.empty
                               Clocks.infinity
                     )
        `shouldBe` Nothing
      describe "後手"
        $          it "Nothing"
        $          Position.move
                     (F5, R5)
                     False
                     (F5, R7)
                     3
                     (Position White
                               (Board.fromList [((F5, R5), Piece White Pawn)])
                               Stands.empty
                               Clocks.infinity
                     )
        `shouldBe` Nothing
      describe "駒を取れない場合" $ do
        describe "移動先に味方の駒がある場合" $ do
          describe "先手"
            $          it "Nothing"
            $          Position.move
                         (F5, R5)
                         False
                         (F5, R4)
                         3
                         (Position
                           Black
                           (Board.fromList
                             [((F5, R5), Piece Black Pawn), ((F5, R4), Piece Black Pawn)]
                           )
                           Stands.empty
                           Clocks.infinity
                         )
            `shouldBe` Nothing
          describe "後手"
            $          it "Nothing"
            $          Position.move
                         (F5, R5)
                         False
                         (F5, R6)
                         3
                         (Position
                           White
                           (Board.fromList
                             [((F5, R5), Piece White Pawn), ((F5, R6), Piece White Pawn)]
                           )
                           Stands.empty
                           Clocks.infinity
                         )
            `shouldBe` Nothing
        describe "移動すると王手になる場合" $ do
          describe "先手"
            $          it "Nothing"
            $          Position.move
                         (F5, R5)
                         False
                         (F4, R5)
                         3
                         (Position
                           Black
                           (Board.fromList
                             [ ((F5, R5), Piece Black Pawn)
                             , ((F5, R1), Piece White Lance)
                             ]
                           )
                           Stands.empty
                           Clocks.infinity
                         )
            `shouldBe` Nothing
          describe "後手"
            $          it "Nothing"
            $          Position.move
                         (F5, R5)
                         False
                         (F6, R5)
                         3
                         (Position
                           White
                           (Board.fromList
                             [ ((F5, R5), Piece White Pawn)
                             , ((F5, R9), Piece Black Lance)
                             ]
                           )
                           Stands.empty
                           Clocks.infinity
                         )
            `shouldBe` Nothing
        describe "持ち時間が無い場合" $ do
          describe "先手"
            $          it "Nothing"
            $          Position.move
                         (F5, R5)
                         False
                         (F4, R5)
                         3
                         (Position
                           Black
                           (Board.fromList
                             [ ((F5, R5), Piece Black Pawn)
                             , ((F5, R1), Piece White Lance)
                             ]
                           )
                           Stands.empty
                           (Clocks Clocks.Timeout Infinity)
                         )
            `shouldBe` Nothing
          describe "後手"
            $          it "Nothing"
            $          Position.move
                         (F5, R5)
                         False
                         (F6, R5)
                         3
                         (Position
                           White
                           (Board.fromList
                             [ ((F5, R5), Piece White Pawn)
                             , ((F5, R9), Piece Black Lance)
                             ]
                           )
                           Stands.empty
                           (Clocks Infinity Clocks.Timeout)
                         )
            `shouldBe` Nothing
  describe "movables" $ do
    describe "王手されていない場合" $ do
      describe "玉将が無い場合" $ do
        describe "先手"
          $          it "可動範囲を返す"
          $          Position.movables
                       (Position
                         Black
                         (Board.fromList
                           [((F5, R9), Piece Black Pawn), ((F5, R1), Piece White Pawn)]
                         )
                         Stands.empty
                         Clocks.infinity
                       )
          `shouldBe` Movables.fromList [((F5, R9), [((F5, R8), No)])]
        describe "後手"
          $          it "可動範囲を返す"
          $          Position.movables
                       (Position
                         White
                         (Board.fromList
                           [((F5, R9), Piece Black Pawn), ((F5, R1), Piece White Pawn)]
                         )
                         Stands.empty
                         Clocks.infinity
                       )
          `shouldBe` Movables.fromList [((F5, R1), [((F5, R2), No)])]
      describe "先手"
        $          it "可動範囲を返す"
        $          Position.movables
                     (Position
                       Black
                       (Board.fromList
                         [((F5, R9), Piece Black King), ((F5, R1), Piece White King)]
                       )
                       Stands.empty
                       Clocks.infinity
                     )
        `shouldBe` Movables.fromList
                     [ ( (F5, R9)
                       , [ ((F6, R8), No)
                         , ((F5, R8), No)
                         , ((F4, R8), No)
                         , ((F6, R9), No)
                         , ((F4, R9), No)
                         ]
                       )
                     ]
      describe "後手"
        $          it "可動範囲を返す"
        $          Position.movables
                     (Position
                       White
                       (Board.fromList
                         [((F5, R9), Piece Black King), ((F5, R1), Piece White King)]
                       )
                       Stands.empty
                       Clocks.infinity
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
                     ]
    describe "王手されている場合" $ do
      describe "先手"
        $          it "王手回避する可動範囲を返す"
        $          Position.movables
                     (Position
                       Black
                       (Board.fromList
                         [((F5, R9), Piece Black King), ((F5, R8), Piece White Gold)]
                       )
                       Stands.empty
                       Clocks.infinity
                     )
        `shouldBe` Movables.fromList [((F5, R9), [((F5, R8), No)])]
      describe "後手"
        $          it "王手回避する可動範囲を返す"
        $          Position.movables
                     (Position
                       White
                       (Board.fromList
                         [((F5, R1), Piece White King), ((F5, R2), Piece Black Gold)]
                       )
                       Stands.empty
                       Clocks.infinity
                     )
        `shouldBe` Movables.fromList [((F5, R1), [((F5, R2), No)])]

  describe "droppables" $ do
    describe "王手されていない場合" $ do
      describe "先手"
        $          it "打ち先範囲を返す"
        $          Position.droppables
                     (Position
                       Black
                       (Board.fromList
                         [((F5, R9), Piece Black King), ((F5, R1), Piece White King)]
                       )
                       (Stands.fromList [(Pawn, 1)] [])
                       Clocks.infinity
                     )
        `shouldBe` Droppables.fromList
                     [ ( Pawn
                       , [ (file, rank)
                         | file <- [F9 .. F1]
                         , rank <- [R2 .. R9]
                         , (file, rank) /= (F5, R9) && (file, rank) /= (F5, R1)
                         ]
                       )
                     ]
      describe "後手"
        $          it "打ち先範囲を返す"
        $          Position.droppables
                     (Position
                       White
                       (Board.fromList
                         [((F5, R9), Piece Black King), ((F5, R1), Piece White King)]
                       )
                       (Stands.fromList [] [(Pawn, 1)])
                       Clocks.infinity
                     )
        `shouldBe` Droppables.fromList
                     [ ( Pawn
                       , [ (file, rank)
                         | file <- [F9 .. F1]
                         , rank <- [R1 .. R8]
                         , (file, rank) /= (F5, R9) && (file, rank) /= (F5, R1)
                         ]
                       )
                     ]
    describe "王手されている場合" $ do
      describe "先手"
        $          it "打ち先範囲を返す"
        $          Position.droppables
                     (Position
                       Black
                       (Board.fromList
                         [ ((F5, R9), Piece Black King)
                         , ((F5, R5), Piece White Lance)
                         , ((F5, R1), Piece White King)
                         ]
                       )
                       (Stands.fromList [(Pawn, 1)] [])
                       Clocks.infinity
                     )
        `shouldBe` Droppables.fromList
                     [(Pawn, [ (F5, rank) | rank <- [R6 .. R8] ])]
      describe "後手"
        $          it "打ち先範囲を返す"
        $          Position.droppables
                     (Position
                       White
                       (Board.fromList
                         [ ((F5, R9), Piece Black King)
                         , ((F5, R5), Piece Black Lance)
                         , ((F5, R1), Piece White King)
                         ]
                       )
                       (Stands.fromList [] [(Pawn, 1)])
                       Clocks.infinity
                     )
        `shouldBe` Droppables.fromList
                     [(Pawn, [ (F5, rank) | rank <- [R2 .. R4] ])]

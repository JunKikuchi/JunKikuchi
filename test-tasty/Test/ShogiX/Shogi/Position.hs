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
  describe "mate" $ do
    describe "詰んでいる場合" $ do
      describe "先手"
        $          it "True"
        $          Position.mate
                     (Position
                       Black
                       (Board.fromList
                         [ ((F5, R9), Piece Black King)
                         , ((F5, R8), Piece White Gold)
                         , ((F5, R7), Piece White Pawn)
                         ]
                       )
                       (Stands.fromTuple ([(Pawn, 1)], [(Pawn, 1)]))
                       Clocks.infinity
                     )
        `shouldBe` True
      describe "後手"
        $          it "True"
        $          Position.mate
                     (Position
                       White
                       (Board.fromList
                         [ ((F5, R1), Piece White King)
                         , ((F5, R2), Piece Black Gold)
                         , ((F5, R3), Piece Black Pawn)
                         ]
                       )
                       (Stands.fromTuple ([(Pawn, 1)], [(Pawn, 1)]))
                       Clocks.infinity
                     )
        `shouldBe` True
      describe "行き場所が無い場合" $ do
        describe "先手"
          $          it "True"
          $          Position.mate
                       (Position
                         Black
                         (Board.fromList
                           [ ((F5, R9), Piece Black King)
                           , ((F5, R7), Piece White Gold)
                           , ((F6, R7), Piece White Lance)
                           , ((F4, R7), Piece White Lance)
                           ]
                         )
                         Stands.empty
                         Clocks.infinity
                       )
          `shouldBe` True
        describe "後手"
          $          it "True"
          $          Position.mate
                       (Position
                         White
                         (Board.fromList
                           [ ((F5, R1), Piece White King)
                           , ((F5, R3), Piece Black Gold)
                           , ((F6, R3), Piece Black Lance)
                           , ((F4, R3), Piece Black Lance)
                           ]
                         )
                         Stands.empty
                         Clocks.infinity
                       )
          `shouldBe` True
    describe "詰んでいない場合" $ do
      describe "移動すると回避" $ do
        describe "先手"
          $          it "False"
          $          Position.mate
                       (Position
                         Black
                         (Board.fromList
                           [((F5, R9), Piece Black King), ((F5, R8), Piece White Gold)]
                         )
                         Stands.empty
                         Clocks.infinity
                       )
          `shouldBe` False
        describe "後手"
          $          it "False"
          $          Position.mate
                       (Position
                         White
                         (Board.fromList
                           [((F5, R1), Piece White King), ((F5, R2), Piece Black Gold)]
                         )
                         Stands.empty
                         Clocks.infinity
                       )
          `shouldBe` False
      describe "駒を打つと詰み回避" $ do
        describe "先手"
          $          it "False"
          $          Position.mate
                       (Position
                         Black
                         (Board.fromList
                           [ ((F5, R9), Piece Black King)
                           , ((F5, R7), Piece White Lance)
                           , ((F6, R7), Piece White Lance)
                           , ((F4, R7), Piece White Lance)
                           ]
                         )
                         (Stands.fromTuple ([(Pawn, 1)], []))
                         Clocks.infinity
                       )
          `shouldBe` False
        describe "後手"
          $          it "False"
          $          Position.mate
                       (Position
                         White
                         (Board.fromList
                           [ ((F5, R1), Piece White King)
                           , ((F5, R3), Piece Black Lance)
                           , ((F6, R3), Piece Black Lance)
                           , ((F4, R3), Piece Black Lance)
                           ]
                         )
                         (Stands.fromTuple ([], [(Pawn, 1)]))
                         Clocks.infinity
                       )
          `shouldBe` False
  describe "move" $ do
    describe "駒の移動ができる場合" $ do
      describe "先手"
        $          it "移動後の局面を返す"
        $          Position.move
                     (F5, R5)
                     False
                     (F5, R4)
                     (Position Black
                               (Board.fromList [((F5, R5), Piece Black Pawn)])
                               Stands.empty
                               (Clocks.guillotine 10)
                     )
        `shouldBe` Right
                     (Position White
                               (Board.fromList [((F5, R4), Piece Black Pawn)])
                               Stands.empty
                               (Clocks.guillotine 10)
                     )
      describe "後手"
        $          it "移動後の局面を返す"
        $          Position.move
                     (F5, R5)
                     False
                     (F5, R6)
                     (Position White
                               (Board.fromList [((F5, R5), Piece White Pawn)])
                               Stands.empty
                               (Clocks.guillotine 10)
                     )
        `shouldBe` Right
                     (Position Black
                               (Board.fromList [((F5, R6), Piece White Pawn)])
                               Stands.empty
                               (Clocks.guillotine 10)
                     )
      describe "駒を取った場合" $ do
        describe "先手"
          $          it "移動後の局面を返す"
          $          Position.move
                       (F5, R5)
                       False
                       (F5, R4)
                       (Position
                         Black
                         (Board.fromList
                           [((F5, R5), Piece Black Pawn), ((F5, R4), Piece White Pawn)]
                         )
                         Stands.empty
                         Clocks.infinity
                       )
          `shouldBe` Right
                       (Position
                         White
                         (Board.fromList [((F5, R4), Piece Black Pawn)])
                         (Stands.fromTuple ([(Pawn, 1)], []))
                         Clocks.infinity
                       )
        describe "後手"
          $          it "移動後の局面を返す"
          $          Position.move
                       (F5, R5)
                       False
                       (F5, R6)
                       (Position
                         White
                         (Board.fromList
                           [((F5, R5), Piece White Pawn), ((F5, R6), Piece Black Pawn)]
                         )
                         Stands.empty
                         Clocks.infinity
                       )
          `shouldBe` Right
                       (Position
                         Black
                         (Board.fromList [((F5, R6), Piece White Pawn)])
                         (Stands.fromTuple ([], [(Pawn, 1)]))
                         Clocks.infinity
                       )
    describe "駒の移動が出来ない場合" $ do
      describe "駒移動違反" $ do
        describe "先手"
          $          it "Illegal IllegalMove"
          $          Position.move
                       (F5, R5)
                       False
                       (F5, R3)
                       (Position Black
                                 (Board.fromList [((F5, R5), Piece Black Pawn)])
                                 Stands.empty
                                 Clocks.infinity
                       )
          `shouldBe` Left (Illegal IllegalMove)
        describe "後手"
          $          it "Illegal IllegalMove"
          $          Position.move
                       (F5, R5)
                       False
                       (F5, R7)
                       (Position White
                                 (Board.fromList [((F5, R5), Piece White Pawn)])
                                 Stands.empty
                                 Clocks.infinity
                       )
          `shouldBe` Left (Illegal IllegalMove)
      describe "移動先に味方の駒がある場合" $ do
        describe "先手"
          $          it "Illegal IllegalMove"
          $          Position.move
                       (F5, R5)
                       False
                       (F5, R4)
                       (Position
                         Black
                         (Board.fromList
                           [((F5, R5), Piece Black Pawn), ((F5, R4), Piece Black Pawn)]
                         )
                         Stands.empty
                         Clocks.infinity
                       )
          `shouldBe` Left (Illegal IllegalMove)
        describe "後手"
          $          it "Illegal IllegalMove"
          $          Position.move
                       (F5, R5)
                       False
                       (F5, R6)
                       (Position
                         White
                         (Board.fromList
                           [((F5, R5), Piece White Pawn), ((F5, R6), Piece White Pawn)]
                         )
                         Stands.empty
                         Clocks.infinity
                       )
          `shouldBe` Left (Illegal IllegalMove)
      describe "移動すると王手になる場合" $ do
        describe "先手"
          $          it "Illegal AbandonCheck"
          $          Position.move
                       (F5, R5)
                       False
                       (F4, R5)
                       (Position
                         Black
                         (Board.fromList
                           [ ((F5, R9), Piece Black King)
                           , ((F5, R5), Piece Black Gold)
                           , ((F5, R1), Piece White Lance)
                           ]
                         )
                         Stands.empty
                         Clocks.infinity
                       )
          `shouldBe` Left (Illegal AbandonCheck)
        describe "後手"
          $          it "Illegal AbandonCheck"
          $          Position.move
                       (F5, R5)
                       False
                       (F6, R5)
                       (Position
                         White
                         (Board.fromList
                           [ ((F5, R1), Piece White King)
                           , ((F5, R5), Piece White Gold)
                           , ((F5, R9), Piece Black Lance)
                           ]
                         )
                         Stands.empty
                         Clocks.infinity
                       )
          `shouldBe` Left (Illegal AbandonCheck)
  describe "drop" $ do
    describe "駒を打ち込める場合" $ do
      describe "先手"
        $          it "打ち込み後の局面を返す"
        $          Position.drop
                     Pawn
                     (F4, R5)
                     (Position Black
                               (Board.fromList [((F5, R5), Piece Black Pawn)])
                               (Stands.fromTuple ([(Pawn, 1)], []))
                               (Clocks.guillotine 10)
                     )
        `shouldBe` Right
                     (Position
                       White
                       (Board.fromList
                         [ ((F5, R5), Piece Black Pawn)
                         , ((F4, R5), Piece Black Pawn)
                         ]
                       )
                       Stands.empty
                       (Clocks.guillotine 10)
                     )
      describe "後手"
        $          it "打ち込み後の局面を返す"
        $          Position.drop
                     Pawn
                     (F6, R5)
                     (Position White
                               (Board.fromList [((F5, R5), Piece White Pawn)])
                               (Stands.fromTuple ([], [(Pawn, 1)]))
                               (Clocks.guillotine 10)
                     )
        `shouldBe` Right
                     (Position
                       Black
                       (Board.fromList
                         [ ((F5, R5), Piece White Pawn)
                         , ((F6, R5), Piece White Pawn)
                         ]
                       )
                       Stands.empty
                       (Clocks.guillotine 10)
                     )
      describe "歩を打ち込んで王手しても詰んでいない場合" $ do
        describe "先手"
          $          it "打ち込み後の局面を返す"
          $          Position.drop
                       Pawn
                       (F5, R2)
                       (Position
                         Black
                         (Board.fromList
                           [ ((F5, R1), Piece White King)
                           , ((F5, R3), Piece Black Gold)
                           , ((F6, R3), Piece Black Lance)
                           ]
                         )
                         (Stands.fromTuple ([(Pawn, 1)], []))
                         Clocks.infinity
                       )
          `shouldBe` Right
                       (Position
                         White
                         (Board.fromList
                           [ ((F5, R1), Piece White King)
                           , ((F5, R3), Piece Black Gold)
                           , ((F6, R3), Piece Black Lance)
                           , ((F5, R2), Piece Black Pawn)
                           ]
                         )
                         Stands.empty
                         Clocks.infinity
                       )
        describe "後手"
          $          it "打ち込み後の局面を返す"
          $          Position.drop
                       Pawn
                       (F5, R8)
                       (Position
                         White
                         (Board.fromList
                           [ ((F5, R9), Piece Black King)
                           , ((F5, R7), Piece White Gold)
                           , ((F6, R7), Piece White Lance)
                           ]
                         )
                         (Stands.fromTuple ([], [(Pawn, 1)]))
                         Clocks.infinity
                       )
          `shouldBe` Right
                       (Position
                         Black
                         (Board.fromList
                           [ ((F5, R9), Piece Black King)
                           , ((F5, R7), Piece White Gold)
                           , ((F6, R7), Piece White Lance)
                           , ((F5, R8), Piece White Pawn)
                           ]
                         )
                         Stands.empty
                         Clocks.infinity
                       )
    describe "駒を打ち込めない場合" $ do
      describe "駒を持っていない場合" $ do
        describe "先手"
          $          it "Illegal IllegalDrop"
          $          Position.drop
                       Pawn
                       (F4, R5)
                       (Position Black
                                 (Board.fromList [((F5, R5), Piece Black Pawn)])
                                 (Stands.fromTuple ([], []))
                                 (Clocks.guillotine 10)
                       )
          `shouldBe` Left (Illegal IllegalDrop)
        describe "後手"
          $          it "Illegal IllegalDrop"
          $          Position.drop
                       Pawn
                       (F6, R5)
                       (Position White
                                 (Board.fromList [((F5, R5), Piece White Pawn)])
                                 (Stands.fromTuple ([], []))
                                 (Clocks.guillotine 10)
                       )
          `shouldBe` Left (Illegal IllegalDrop)
      describe "指定された駒を持っていない場合" $ do
        describe "先手"
          $          it "Illegal IllegalDrop"
          $          Position.drop
                       Pawn
                       (F4, R5)
                       (Position Black
                                 (Board.fromList [((F5, R5), Piece Black Pawn)])
                                 (Stands.fromTuple ([(Gold, 1)], [(Gold, 1)]))
                                 (Clocks.guillotine 10)
                       )
          `shouldBe` Left (Illegal IllegalDrop)
        describe "後手"
          $          it "Illegal IllegalDrop"
          $          Position.drop
                       Pawn
                       (F6, R5)
                       (Position White
                                 (Board.fromList [((F5, R5), Piece White Pawn)])
                                 (Stands.fromTuple ([(Gold, 1)], [(Gold, 1)]))
                                 (Clocks.guillotine 10)
                       )
          `shouldBe` Left (Illegal IllegalDrop)
      describe "駒を打ち込めないマス目の場合" $ do
        describe "先手"
          $          it "Illegal IllegalDrop"
          $          Position.drop
                       Pawn
                       (F5, R1)
                       (Position Black
                                 (Board.fromList [])
                                 (Stands.fromTuple ([(Pawn, 1)], []))
                                 (Clocks.guillotine 10)
                       )
          `shouldBe` Left (Illegal IllegalDrop)
        describe "後手"
          $          it "Illegal IllegalDrop"
          $          Position.drop
                       Pawn
                       (F5, R9)
                       (Position White
                                 (Board.fromList [])
                                 (Stands.fromTuple ([], [(Pawn, 1)]))
                                 (Clocks.guillotine 10)
                       )
          `shouldBe` Left (Illegal IllegalDrop)
      describe "二歩の場合" $ do
        describe "先手"
          $          it "Illegal IllegalDrop"
          $          Position.drop
                       Pawn
                       (F5, R5)
                       (Position Black
                                 (Board.fromList [((F5, R9), Piece Black Pawn)])
                                 (Stands.fromTuple ([(Pawn, 1)], []))
                                 Clocks.infinity
                       )
          `shouldBe` Left (Illegal IllegalDrop)
        describe "後手"
          $          it "Illegal IllegalDrop"
          $          Position.drop
                       Pawn
                       (F5, R5)
                       (Position White
                                 (Board.fromList [((F5, R1), Piece White Pawn)])
                                 (Stands.fromTuple ([], [(Pawn, 1)]))
                                 Clocks.infinity
                       )
          `shouldBe` Left (Illegal IllegalDrop)
      describe "王手を回避出来ない場合" $ do
        describe "先手"
          $          it "Illegal AbandonCheck"
          $          Position.drop
                       Pawn
                       (F4, R5)
                       (Position
                         Black
                         (Board.fromList
                           [((F5, R9), Piece Black King), ((F5, R1), Piece White Lance)]
                         )
                         (Stands.fromTuple ([(Pawn, 1)], [(Pawn, 1)]))
                         Clocks.infinity
                       )
          `shouldBe` Left (Illegal AbandonCheck)
        describe "後手"
          $          it "Illegal AbandonCheck"
          $          Position.drop
                       Pawn
                       (F6, R5)
                       (Position
                         White
                         (Board.fromList
                           [((F5, R1), Piece White King), ((F5, R9), Piece Black Lance)]
                         )
                         (Stands.fromTuple ([(Pawn, 1)], [(Pawn, 1)]))
                         Clocks.infinity
                       )
          `shouldBe` Left (Illegal AbandonCheck)
      describe "打ち歩詰めの場合" $ do
        describe "先手"
          $          it "Illegal DroppedPawnMate"
          $          Position.drop
                       Pawn
                       (F1, R4)
                       (Position
                         Black
                         (Board.fromList
                           [ ((F2, R1), Piece Black PromotedBishop)
                           , ((F2, R5), Piece Black Silver)
                           , ((F2, R3), Piece White Pawn)
                           , ((F1, R3), Piece White King)
                           ]
                         )
                         (Stands.fromTuple ([(Pawn, 1)], []))
                         Clocks.infinity
                       )
          `shouldBe` Left (Illegal DroppedPawnMate)
        describe "後手"
          $          it "Illegal DroppedPawnMate"
          $          Position.drop
                       Pawn
                       (F1, R6)
                       (Position
                         White
                         (Board.fromList
                           [ ((F1, R7), Piece Black King)
                           , ((F2, R7), Piece Black Pawn)
                           , ((F2, R9), Piece White PromotedBishop)
                           , ((F2, R5), Piece White Silver)
                           ]
                         )
                         (Stands.fromTuple ([], [(Pawn, 1)]))
                         Clocks.infinity
                       )
          `shouldBe` Left (Illegal DroppedPawnMate)
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
                       (Stands.fromTuple ([(Pawn, 1)], []))
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
                       (Stands.fromTuple ([], [(Pawn, 1)]))
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
                       (Stands.fromTuple ([(Pawn, 1)], []))
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
                       (Stands.fromTuple ([], [(Pawn, 1)]))
                       Clocks.infinity
                     )
        `shouldBe` Droppables.fromList
                     [(Pawn, [ (F5, rank) | rank <- [R2 .. R4] ])]

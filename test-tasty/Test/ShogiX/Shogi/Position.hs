module Test.ShogiX.Shogi.Position where

import           RIO
import qualified RIO.Map                       as Map
import           Test.Tasty
import           Test.Tasty.Hspec
import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Position         as Position
import qualified ShogiX.Shogi.Stand            as Stand
import           ShogiX.Clocks                 as Clocks

{-# ANN module "HLint: ignore Use camelCase" #-}
spec_movables :: Spec
spec_movables = describe "movables" $ do
  describe "王手されていない場合" $ do
    describe "玉将が無い場合" $ do
      describe "先手"
        $          it "可動範囲を返す"
      $          Position.movables
                   (Position
                     Black
                     (Board
                       (Map.fromList
                         [((F5, R9), Piece Black Pawn), ((F5, R1), Piece White Pawn)]
                       )
                     )
                     (Stands Stand.empty Stand.empty)
                     Clocks.infinity
                   )
      `shouldBe` Movables
                   (Map.fromList
                     [((F5, R9), Movable (Map.fromList [((F5, R8), No)]))]
                   )
      describe "後手"
        $          it "可動範囲を返す"
        $          Position.movables
                     (Position
                       White
                       (Board
                         (Map.fromList
                           [((F5, R9), Piece Black Pawn), ((F5, R1), Piece White Pawn)]
                         )
                       )
                       (Stands Stand.empty Stand.empty)
                       Clocks.infinity
                     )
        `shouldBe` Movables
                     (Map.fromList
                       [((F5, R1), Movable (Map.fromList [((F5, R2), No)]))]
                     )
    describe "先手"
      $          it "可動範囲を返す"
      $          Position.movables
                   (Position
                     Black
                     (Board
                       (Map.fromList
                         [((F5, R9), Piece Black King), ((F5, R1), Piece White King)]
                       )
                     )
                     (Stands Stand.empty Stand.empty)
                     Clocks.infinity
                   )
      `shouldBe` Movables
                   (Map.fromList
                     [ ( (F5, R9)
                       , Movable
                         (Map.fromList
                           [ ((F6, R8), No)
                           , ((F5, R8), No)
                           , ((F4, R8), No)
                           , ((F6, R9), No)
                           , ((F4, R9), No)
                           ]
                         )
                       )
                     ]
                   )
    describe "後手"
      $          it "可動範囲を返す"
      $          Position.movables
                   (Position
                     White
                     (Board
                       (Map.fromList
                         [((F5, R9), Piece Black King), ((F5, R1), Piece White King)]
                       )
                     )
                     (Stands Stand.empty Stand.empty)
                     Clocks.infinity
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
                     ]
                   )
  describe "王手されている場合" $ do
    describe "先手"
      $          it "王手回避する可動範囲を返す"
      $          Position.movables
                   (Position
                     Black
                     (Board
                       (Map.fromList
                         [((F5, R9), Piece Black King), ((F5, R8), Piece White Gold)]
                       )
                     )
                     (Stands Stand.empty Stand.empty)
                     Clocks.infinity
                   )
      `shouldBe` Movables
                   (Map.fromList
                     [((F5, R9), Movable (Map.fromList [((F5, R8), No)]))]
                   )
    describe "後手"
      $          it "王手回避する可動範囲を返す"
      $          Position.movables
                   (Position
                     White
                     (Board
                       (Map.fromList
                         [((F5, R1), Piece White King), ((F5, R2), Piece Black Gold)]
                       )
                     )
                     (Stands Stand.empty Stand.empty)
                     Clocks.infinity
                   )
      `shouldBe` Movables
                   (Map.fromList
                     [((F5, R1), Movable (Map.fromList [((F5, R2), No)]))]
                   )

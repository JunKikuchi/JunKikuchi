module Test.ShogiX.Shogi.Piece where

import           RIO
import           Test.Tasty
import           Test.Tasty.Hspec
import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Piece            as Piece

{-# ANN module "HLint: ignore Use camelCase" #-}
spec_Pawn :: Spec
spec_Pawn = describe "歩兵の可動範囲" $ do
  describe "先手" $ do
    it "前あり" $ Piece.pawn Black (F5, R5) `shouldBe` [[(F5, R4)]]
    it "前なし" $ Piece.pawn Black (F5, R1) `shouldBe` [[]]
  describe "後手" $ do
    it "前あり" $ Piece.pawn White (F5, R5) `shouldBe` [[(F5, R6)]]
    it "前なし" $ Piece.pawn White (F5, R9) `shouldBe` [[]]

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

spec_Lance :: Spec
spec_Lance = describe "香車の可動範囲" $ do
  describe "先手" $ do
    it "前あり"
      $          Piece.lance Black (F5, R5)
      `shouldBe` [[(F5, R4), (F5, R3), (F5, R2), (F5, R1)]]
    it "前なし" $ Piece.lance Black (F5, R1) `shouldBe` [[]]
  describe "後手" $ do
    it "前あり"
      $          Piece.lance White (F5, R5)
      `shouldBe` [[(F5, R6), (F5, R7), (F5, R8), (F5, R9)]]
    it "前なし" $ Piece.lance White (F5, R9) `shouldBe` [[]]

spec_Knight :: Spec
spec_Knight = describe "桂馬の可動範囲" $ do
  describe "先手" $ do
    it "前両方あり" $ Piece.knight Black (F5, R5) `shouldBe` [[(F6, R3)], [(F4, R3)]]
    it "前右あり" $ Piece.knight Black (F9, R5) `shouldBe` [[], [(F8, R3)]]
    it "前左あり" $ Piece.knight Black (F1, R5) `shouldBe` [[(F2, R3)], []]
    it "前なし" $ Piece.knight Black (F5, R2) `shouldBe` [[], []]
  describe "後手" $ do
    it "前両方あり" $ Piece.knight White (F5, R5) `shouldBe` [[(F4, R7)], [(F6, R7)]]
    it "前右あり" $ Piece.knight White (F9, R5) `shouldBe` [[(F8, R7)], []]
    it "前左あり" $ Piece.knight White (F1, R5) `shouldBe` [[], [(F2, R7)]]
    it "前なし" $ Piece.knight White (F5, R8) `shouldBe` [[], []]

spec_Silver :: Spec
spec_Silver = describe "銀将の可動範囲" $ do
  describe "先手"
    $          it "左前-前-右前-右下-左下あり"
    $          Piece.silver Black (F5, R5)
    `shouldBe` [[(F6, R4)], [(F5, R4)], [(F4, R4)], [(F4, R6)], [(F6, R6)]]
  describe "後手"
    $          it "左前-前-右前-右下-左下あり"
    $          Piece.silver White (F5, R5)
    `shouldBe` [[(F6, R6)], [(F5, R6)], [(F4, R6)], [(F4, R4)], [(F6, R4)]]

spec_Gold :: Spec
spec_Gold = describe "金将の可動範囲" $ do
  describe "先手"
    $          it "左前-前-右前-右-下-左あり"
    $          Piece.gold Black (F5, R5)
    `shouldBe` [ [(F6, R4)]
               , [(F5, R4)]
               , [(F4, R4)]
               , [(F4, R5)]
               , [(F5, R6)]
               , [(F6, R5)]
               ]
  describe "後手"
    $          it "左前-前-右前-右-下-左あり"
    $          Piece.gold White (F5, R5)
    `shouldBe` [ [(F6, R6)]
               , [(F5, R6)]
               , [(F4, R6)]
               , [(F4, R5)]
               , [(F5, R4)]
               , [(F6, R5)]
               ]

module Test.ShogiX.Shogi.Piece where

import           RIO
import qualified RIO.Map                       as Map
import           Test.Tasty
import           Test.Tasty.Hspec
import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Piece            as Piece

{-# ANN module "HLint: ignore Use camelCase" #-}
spec_movable :: Spec
spec_movable = describe "movable" $ do
  describe "歩兵" $ do
    it "先手"
      $          Piece.movable (Piece Black Pawn) (F5, R5) Map.empty
      `shouldBe` Movable (Map.fromList [((F5, R4), No)])
    it "後手"
      $          Piece.movable (Piece White Pawn) (F5, R5) Map.empty
      `shouldBe` Movable (Map.fromList [((F5, R6), No)])
  describe "香車" $ do
    it "先手"
      $          Piece.movable (Piece Black Lance) (F5, R5) Map.empty
      `shouldBe` Movable
                   (Map.fromList
                     [ ((F5, R4), No)
                     , ((F5, R3), Option)
                     , ((F5, R2), Option)
                     , ((F5, R1), Must)
                     ]
                   )
    it "後手"
      $          Piece.movable (Piece White Lance) (F5, R5) Map.empty
      `shouldBe` Movable
                   (Map.fromList
                     [ ((F5, R6), No)
                     , ((F5, R7), Option)
                     , ((F5, R8), Option)
                     , ((F5, R9), Must)
                     ]
                   )
  describe "桂馬" $ do
    it "先手"
      $          Piece.movable (Piece Black Knight) (F5, R5) Map.empty
      `shouldBe` Movable (Map.fromList [((F6, R3), Option), ((F4, R3), Option)])
    it "後手"
      $          Piece.movable (Piece White Knight) (F5, R5) Map.empty
      `shouldBe` Movable (Map.fromList [((F6, R7), Option), ((F4, R7), Option)])
  describe "銀将" $ do
    it "先手"
      $          Piece.movable (Piece Black Silver) (F5, R5) Map.empty
      `shouldBe` Movable
                   (Map.fromList
                     [ ((F6, R4), No)
                     , ((F5, R4), No)
                     , ((F4, R4), No)
                     , ((F4, R6), No)
                     , ((F6, R6), No)
                     ]
                   )
    it "後手"
      $          Piece.movable (Piece White Silver) (F5, R5) Map.empty
      `shouldBe` Movable
                   (Map.fromList
                     [ ((F6, R6), No)
                     , ((F5, R6), No)
                     , ((F4, R6), No)
                     , ((F4, R4), No)
                     , ((F6, R4), No)
                     ]
                   )
  describe "金将" $ do
    it "先手"
      $          Piece.movable (Piece Black Gold) (F5, R5) Map.empty
      `shouldBe` Movable
                   (Map.fromList
                     [ ((F6, R4), No)
                     , ((F5, R4), No)
                     , ((F4, R4), No)
                     , ((F4, R5), No)
                     , ((F5, R6), No)
                     , ((F6, R5), No)
                     ]
                   )
    it "後手"
      $          Piece.movable (Piece White Gold) (F5, R5) Map.empty
      `shouldBe` Movable
                   (Map.fromList
                     [ ((F6, R6), No)
                     , ((F5, R6), No)
                     , ((F4, R6), No)
                     , ((F4, R5), No)
                     , ((F5, R4), No)
                     , ((F6, R5), No)
                     ]
                   )
  describe "角行" $ do
    it "先手"
      $          Piece.movable (Piece Black Bishop) (F5, R5) Map.empty
      `shouldBe` Movable
                   (Map.fromList
                     [ ((F6, R4), No)
                     , ((F7, R3), Option)
                     , ((F8, R2), Option)
                     , ((F9, R1), Option)
                     , ((F4, R4), No)
                     , ((F3, R3), Option)
                     , ((F2, R2), Option)
                     , ((F1, R1), Option)
                     , ((F4, R6), No)
                     , ((F3, R7), No)
                     , ((F2, R8), No)
                     , ((F1, R9), No)
                     , ((F6, R6), No)
                     , ((F7, R7), No)
                     , ((F8, R8), No)
                     , ((F9, R9), No)
                     ]
                   )
    it "後手"
      $          Piece.movable (Piece White Bishop) (F5, R5) Map.empty
      `shouldBe` Movable
                   (Map.fromList
                     [ ((F6, R4), No)
                     , ((F7, R3), No)
                     , ((F8, R2), No)
                     , ((F9, R1), No)
                     , ((F4, R4), No)
                     , ((F3, R3), No)
                     , ((F2, R2), No)
                     , ((F1, R1), No)
                     , ((F4, R6), No)
                     , ((F3, R7), Option)
                     , ((F2, R8), Option)
                     , ((F1, R9), Option)
                     , ((F6, R6), No)
                     , ((F7, R7), Option)
                     , ((F8, R8), Option)
                     , ((F9, R9), Option)
                     ]
                   )
  describe "飛車" $ do
    it "先手"
      $          Piece.movable (Piece Black Rook) (F5, R5) Map.empty
      `shouldBe` Movable
                   (Map.fromList
                     [ ((F5, R4), No)
                     , ((F5, R3), Option)
                     , ((F5, R2), Option)
                     , ((F5, R1), Option)
                     , ((F4, R5), No)
                     , ((F3, R5), No)
                     , ((F2, R5), No)
                     , ((F1, R5), No)
                     , ((F5, R6), No)
                     , ((F5, R7), No)
                     , ((F5, R8), No)
                     , ((F5, R9), No)
                     , ((F6, R5), No)
                     , ((F7, R5), No)
                     , ((F8, R5), No)
                     , ((F9, R5), No)
                     ]
                   )
    it "後手"
      $          Piece.movable (Piece White Rook) (F5, R5) Map.empty
      `shouldBe` Movable
                   (Map.fromList
                     [ ((F5, R4), No)
                     , ((F5, R3), No)
                     , ((F5, R2), No)
                     , ((F5, R1), No)
                     , ((F4, R5), No)
                     , ((F3, R5), No)
                     , ((F2, R5), No)
                     , ((F1, R5), No)
                     , ((F5, R6), No)
                     , ((F5, R7), Option)
                     , ((F5, R8), Option)
                     , ((F5, R9), Option)
                     , ((F6, R5), No)
                     , ((F7, R5), No)
                     , ((F8, R5), No)
                     , ((F9, R5), No)
                     ]
                   )
  describe "玉将" $ do
    it "先手"
      $          Piece.movable (Piece Black King) (F5, R5) Map.empty
      `shouldBe` Movable
                   (Map.fromList
                     [ ((F6, R4), No)
                     , ((F5, R4), No)
                     , ((F4, R4), No)
                     , ((F4, R5), No)
                     , ((F4, R6), No)
                     , ((F5, R6), No)
                     , ((F6, R6), No)
                     , ((F6, R5), No)
                     ]
                   )
    it "後手"
      $          Piece.movable (Piece White King) (F5, R5) Map.empty
      `shouldBe` Movable
                   (Map.fromList
                     [ ((F6, R6), No)
                     , ((F5, R6), No)
                     , ((F4, R6), No)
                     , ((F4, R5), No)
                     , ((F4, R4), No)
                     , ((F5, R4), No)
                     , ((F6, R4), No)
                     , ((F6, R5), No)
                     ]
                   )
  describe "と金" $ do
    it "先手"
      $          Piece.movable (Piece Black PromotedPawn) (F5, R5) Map.empty
      `shouldBe` Piece.movable (Piece Black Gold) (F5, R5) Map.empty
    it "後手"
      $          Piece.movable (Piece White PromotedPawn) (F5, R5) Map.empty
      `shouldBe` Piece.movable (Piece White Gold) (F5, R5) Map.empty
  describe "成香" $ do
    it "先手"
      $          Piece.movable (Piece Black PromotedLance) (F5, R5) Map.empty
      `shouldBe` Piece.movable (Piece Black Gold) (F5, R5) Map.empty
    it "後手"
      $          Piece.movable (Piece White PromotedLance) (F5, R5) Map.empty
      `shouldBe` Piece.movable (Piece White Gold) (F5, R5) Map.empty
  describe "成桂" $ do
    it "先手"
      $          Piece.movable (Piece Black PromotedKnight) (F5, R5) Map.empty
      `shouldBe` Piece.movable (Piece Black Gold) (F5, R5) Map.empty
    it "後手"
      $          Piece.movable (Piece White PromotedKnight) (F5, R5) Map.empty
      `shouldBe` Piece.movable (Piece White Gold) (F5, R5) Map.empty
  describe "成銀" $ do
    it "先手"
      $          Piece.movable (Piece Black PromotedSilver) (F5, R5) Map.empty
      `shouldBe` Piece.movable (Piece Black Gold) (F5, R5) Map.empty
    it "後手"
      $          Piece.movable (Piece White PromotedSilver) (F5, R5) Map.empty
      `shouldBe` Piece.movable (Piece White Gold) (F5, R5) Map.empty

spec_Pawn :: Spec
spec_Pawn = describe "歩兵の可動範囲" $ do
  describe "先手" $ do
    it "前" $ Piece.pawn Black (F5, R5) `shouldBe` [[(F5, R4)]]
    it "なし" $ Piece.pawn Black (F5, R1) `shouldBe` [[]]
  describe "後手" $ do
    it "前" $ Piece.pawn White (F5, R5) `shouldBe` [[(F5, R6)]]
    it "なし" $ Piece.pawn White (F5, R9) `shouldBe` [[]]

spec_Lance :: Spec
spec_Lance = describe "香車の可動範囲" $ do
  describe "先手" $ do
    it "前"
      $          Piece.lance Black (F5, R5)
      `shouldBe` [[(F5, R4), (F5, R3), (F5, R2), (F5, R1)]]
    it "なし" $ Piece.lance Black (F5, R1) `shouldBe` [[]]
  describe "後手" $ do
    it "前"
      $          Piece.lance White (F5, R5)
      `shouldBe` [[(F5, R6), (F5, R7), (F5, R8), (F5, R9)]]
    it "なし" $ Piece.lance White (F5, R9) `shouldBe` [[]]

spec_Knight :: Spec
spec_Knight = describe "桂馬の可動範囲" $ do
  describe "先手" $ do
    it "前両方" $ Piece.knight Black (F5, R5) `shouldBe` [[(F6, R3)], [(F4, R3)]]
    it "前右" $ Piece.knight Black (F9, R5) `shouldBe` [[], [(F8, R3)]]
    it "前左" $ Piece.knight Black (F1, R5) `shouldBe` [[(F2, R3)], []]
    it "なし" $ Piece.knight Black (F5, R2) `shouldBe` [[], []]
  describe "後手" $ do
    it "前両方" $ Piece.knight White (F5, R5) `shouldBe` [[(F4, R7)], [(F6, R7)]]
    it "前右" $ Piece.knight White (F9, R5) `shouldBe` [[(F8, R7)], []]
    it "前左" $ Piece.knight White (F1, R5) `shouldBe` [[], [(F2, R7)]]
    it "なし" $ Piece.knight White (F5, R8) `shouldBe` [[], []]

spec_Silver :: Spec
spec_Silver = describe "銀将の可動範囲" $ do
  describe "先手"
    $          it "左前-前-右前-右下-左下"
    $          Piece.silver Black (F5, R5)
    `shouldBe` [[(F6, R4)], [(F5, R4)], [(F4, R4)], [(F4, R6)], [(F6, R6)]]
  describe "後手"
    $          it "左前-前-右前-右下-左下"
    $          Piece.silver White (F5, R5)
    `shouldBe` [[(F6, R6)], [(F5, R6)], [(F4, R6)], [(F4, R4)], [(F6, R4)]]

spec_Gold :: Spec
spec_Gold = describe "金将の可動範囲" $ do
  describe "先手"
    $          it "左前-前-右前-右-下-左"
    $          Piece.gold Black (F5, R5)
    `shouldBe` [ [(F6, R4)]
               , [(F5, R4)]
               , [(F4, R4)]
               , [(F4, R5)]
               , [(F5, R6)]
               , [(F6, R5)]
               ]
  describe "後手"
    $          it "左前-前-右前-右-下-左"
    $          Piece.gold White (F5, R5)
    `shouldBe` [ [(F6, R6)]
               , [(F5, R6)]
               , [(F4, R6)]
               , [(F4, R5)]
               , [(F5, R4)]
               , [(F6, R5)]
               ]

spec_Bishop :: Spec
spec_Bishop = describe "角行の可動範囲" $ do
  describe "先手"
    $          it "左前連続-右前連続-右下連続-左下連続"
    $          Piece.bishop Black (F5, R5)
    `shouldBe` [ [(F6, R4), (F7, R3), (F8, R2), (F9, R1)]
               , [(F4, R4), (F3, R3), (F2, R2), (F1, R1)]
               , [(F4, R6), (F3, R7), (F2, R8), (F1, R9)]
               , [(F6, R6), (F7, R7), (F8, R8), (F9, R9)]
               ]
  describe "後手"
    $          it "左前連続-右前連続-右下連続-左下連続"
    $          Piece.bishop White (F5, R5)
    `shouldBe` Piece.bishop Black (F5, R5)

spec_Rook :: Spec
spec_Rook = describe "飛車の可動範囲" $ do
  describe "先手"
    $          it "前連続-右連続-下連続-左連続"
    $          Piece.rook Black (F5, R5)
    `shouldBe` [ [(F5, R4), (F5, R3), (F5, R2), (F5, R1)]
               , [(F4, R5), (F3, R5), (F2, R5), (F1, R5)]
               , [(F5, R6), (F5, R7), (F5, R8), (F5, R9)]
               , [(F6, R5), (F7, R5), (F8, R5), (F9, R5)]
               ]
  describe "後手"
    $          it "前連続-右連続-下連続-左連続"
    $          Piece.rook White (F5, R5)
    `shouldBe` Piece.rook Black (F5, R5)

spec_King :: Spec
spec_King = describe "玉将の可動範囲" $ do
  describe "先手"
    $          it "左前-前-右前-右-右下-下-左下-左"
    $          Piece.king Black (F5, R5)
    `shouldBe` [ [(F6, R4)]
               , [(F5, R4)]
               , [(F4, R4)]
               , [(F4, R5)]
               , [(F4, R6)]
               , [(F5, R6)]
               , [(F6, R6)]
               , [(F6, R5)]
               ]
  describe "後手"
    $          it "左前-前-右前-右-右下-下-左下-左"
    $          Piece.king White (F5, R5)
    `shouldBe` [ [(F6, R6)]
               , [(F5, R6)]
               , [(F4, R6)]
               , [(F4, R5)]
               , [(F4, R4)]
               , [(F5, R4)]
               , [(F6, R4)]
               , [(F6, R5)]
               ]

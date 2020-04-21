module Test.ShogiX.Shogi where
import           RIO
import           RIO.NonEmpty                   ( (<|) )
import           Test.Tasty
import           Test.Tasty.Hspec
import           ShogiX.Shogi
import qualified ShogiX.Shogi.Board            as Board
import qualified ShogiX.Shogi.Stands           as Stands
import qualified ShogiX.Clocks                 as Clocks
import qualified ShogiX.Shogi.Position         as Position
import qualified ShogiX.Shogi.Movables         as Movables
import qualified ShogiX.Shogi.Droppables       as Droppables

{-# ANN module "HLint: ignore Use camelCase" #-}
spec_Test_ShogiX_Shogi :: Spec
spec_Test_ShogiX_Shogi = describe "update Move(..)" $ do
  let board = Board.fromList
        [ ((F5, R9), Piece Black King)
        , ((F5, R7), Piece White Pawn)
        , ((F4, R7), Piece White Silver)
        , ((F5, R1), Piece White King)
        , ((F5, R3), Piece Black Pawn)
        , ((F4, R3), Piece Black Silver)
        ]
  let stands   = Stands.fromList [(Gold, 1)] [(Gold, 1)]
  let position = Position Black board stands Clocks.infinity
  let shogi    = Shogi Open (Positions (position :| []))
  describe "駒を移動" $ do
    describe "対局継続" $ do
      let newBoard = Board.fromList
            [ ( (F4, R9)
              , Piece Black King
              ) -- 移動
            , ((F5, R7), Piece White Pawn)
            , ((F4, R7), Piece White Silver)
            , ((F5, R1), Piece White King)
            , ((F5, R3), Piece Black Pawn)
            , ((F4, R3), Piece Black Silver)
            ]
      let newPosition = Position White newBoard stands Clocks.infinity
      let newShogi    = Shogi Open (Positions (newPosition <| position :| []))
      it "将棋データを更新"
        $          update (Move (F5, R9) False (F4, R9)) 3 shogi
        `shouldBe` newShogi
    describe "詰みで対局終了" $ do
      let newBoard = Board.fromList
            [ ((F5, R9), Piece Black King)
            , ((F5, R7), Piece White Pawn)
            , ((F4, R7), Piece White Silver)
            , ((F5, R1), Piece White King)
            , ((F5, R3), Piece Black Pawn)
            , ((F5, R2), Piece Black PromotedSilver) -- 移動
            ]
      let newPosition = Position White newBoard stands Clocks.infinity
      let newShogi = Shogi (Closed Black Mate)
                           (Positions (newPosition <| position :| []))
      it "将棋データを更新"
        $          update (Move (F4, R3) True (F5, R2)) 3 shogi
        `shouldBe` newShogi
  describe "駒を打ち込み" $ do
    describe "対局継続" $ do
      let newBoard = Board.fromList
            [ ((F5, R9), Piece Black King)
            , ((F5, R7), Piece White Pawn)
            , ((F4, R7), Piece White Silver)
            , ((F5, R1), Piece White King)
            , ((F5, R3), Piece Black Pawn)
            , ((F4, R3), Piece Black Silver)
            , ((F4, R9), Piece Black Gold) -- 打ち込み
            ]
      let newStands   = Stands.fromList [] [(Gold, 1)]
      let newPosition = Position White newBoard newStands Clocks.infinity
      let newShogi    = Shogi Open (Positions (newPosition <| position :| []))
      it "将棋データを更新" $ update (Drop Gold (F4, R9)) 3 shogi `shouldBe` newShogi
    describe "詰みで対局終了" $ do
      let newBoard' = Board.fromList
            [ ((F5, R9), Piece Black King)
            , ((F5, R7), Piece White Pawn)
            , ((F4, R7), Piece White Silver)
            , ((F5, R1), Piece White King)
            , ((F5, R3), Piece Black Pawn)
            , ((F4, R3), Piece Black Silver)
            , ((F5, R2), Piece Black Gold) -- 打ち込み
            ]
      let newStands   = Stands.fromList [] [(Gold, 1)]
      let newPosition = Position White newBoard' newStands Clocks.infinity
      let newShogi = Shogi (Closed Black Mate)
                           (Positions (newPosition <| position :| []))
      it "将棋データを更新" $ update (Drop Gold (F5, R2)) 3 shogi `shouldBe` newShogi
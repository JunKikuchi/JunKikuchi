module Test.ShogiX.Shogi where
import           RIO
import           RIO.NonEmpty                   ( (<|) )
import qualified RIO.Set                       as Set
import           Test.Tasty
import           Test.Tasty.Hspec
import           ShogiX.Shogi
import qualified ShogiX.Shogi.Board            as Board
import qualified ShogiX.Shogi.Stands           as Stands
import           ShogiX.Clocks                  ( Clocks(..) )
import qualified ShogiX.Clocks                 as Clocks
import qualified ShogiX.Shogi.Position         as Position
import qualified ShogiX.Shogi.Positions        as Positions
import qualified ShogiX.Shogi.Updates          as Updates
import qualified ShogiX.Shogi.Movables         as Movables
import qualified ShogiX.Shogi.Droppables       as Droppables

{-# ANN module "HLint: ignore Reduce duplication" #-}
{-# ANN module "HLint: ignore Use camelCase" #-}
spec_Test_ShogiX_Shogi :: Spec
spec_Test_ShogiX_Shogi = do
  describe "update" $ do
    let board = Board.fromList
          [ ((F5, R9), Piece Black King)
          , ((F5, R7), Piece White Pawn)
          , ((F4, R7), Piece White Silver)
          , ((F5, R1), Piece White King)
          , ((F5, R3), Piece Black Pawn)
          , ((F4, R3), Piece Black Silver)
          ]
    let stands   = Stands.fromTuple ([(Gold, 1)], [(Gold, 1)])
    let position = Position Black board stands $ Clocks.guillotine 10
    let shogi    = Shogi Open (Positions.singleton position) Updates.empty
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
        let newPosition = Position White newBoard stands
              $ Clocks (Clocks.Guillotine 7) (Clocks.Guillotine 10)
        let newShogi = Shogi
              Open
              (Positions.cons newPosition (Positions.singleton position))
              (Updates [(Move (F5, R9) False (F4, R9), 3)])
        it "将棋データを更新"
          $          update (Move (F5, R9) False (F4, R9)) 3 shogi
          `shouldBe` Just newShogi
      describe "詰みで対局終了" $ do
        let newBoard = Board.fromList
              [ ((F5, R9), Piece Black King)
              , ((F5, R7), Piece White Pawn)
              , ((F4, R7), Piece White Silver)
              , ((F5, R1), Piece White King)
              , ((F5, R3), Piece Black Pawn)
              , ((F5, R2), Piece Black PromotedSilver) -- 移動
              ]
        let newPosition = Position White newBoard stands
              $ Clocks (Clocks.Guillotine 7) (Clocks.Guillotine 10)
        let newShogi = Shogi
              (Closed Black Mate)
              (Positions.cons newPosition (Positions.singleton position))
              (Updates [(Move (F4, R3) True (F5, R2), 3)])
        it "将棋データを更新"
          $          update (Move (F4, R3) True (F5, R2)) 3 shogi
          `shouldBe` Just newShogi
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
        let newStands = Stands.fromTuple ([], [(Gold, 1)])
        let newPosition = Position White newBoard newStands
              $ Clocks (Clocks.Guillotine 7) (Clocks.Guillotine 10)
        let newShogi = Shogi
              Open
              (Positions.cons newPosition (Positions.singleton position))
              (Updates [(Drop Gold (F4, R9), 3)])
        it "将棋データを更新"
          $          update (Drop Gold (F4, R9)) 3 shogi
          `shouldBe` Just newShogi
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
        let newStands = Stands.fromTuple ([], [(Gold, 1)])
        let newPosition = Position White newBoard' newStands
              $ Clocks (Clocks.Guillotine 7) (Clocks.Guillotine 10)
        let newShogi = Shogi
              (Closed Black Mate)
              (Positions.cons newPosition (Positions.singleton position))
              (Updates [(Drop Gold (F5, R2), 3)])
        it "将棋データを更新"
          $          update (Drop Gold (F5, R2)) 3 shogi
          `shouldBe` Just newShogi
    describe "時間切れ" $ do
      let newPosition = Position Black board stands
            $ Clocks Clocks.Timeout (Clocks.Guillotine 10)
      let newShogi = Shogi
            (Closed White ShogiX.Shogi.Timeout)
            (Positions.cons newPosition (Positions.singleton position))
            (Updates [(Move (F4, R3) True (F5, R2), 10)])
      it "将棋データを更新"
        $          update (Move (F4, R3) True (F5, R2)) 10 shogi
        `shouldBe` Just newShogi
    describe "投了" $ do
      let newPosition = Position Black board stands
            $ Clocks (Clocks.Guillotine 7) (Clocks.Guillotine 10)
      let newShogi = Shogi
            (Closed White Resign)
            (Positions.cons newPosition (Positions.singleton position))
            (Updates [(CloseResign, 3)])
      it "将棋データを更新" $ update CloseResign 3 shogi `shouldBe` Just newShogi
    describe "対局時計の時間を進める" $ do
      describe "残り時間あり"
        $          it "将棋データをそのまま返す"
        $          update ConsumeTime 3 shogi
        `shouldBe` Just shogi
      describe "時間切れ" $ do
        let newPosition = Position Black board stands
              $ Clocks Clocks.Timeout (Clocks.Guillotine 10)
        let newShogi = Shogi
              (Closed White ShogiX.Shogi.Timeout)
              (Positions.cons newPosition (Positions.singleton position))
              (Updates [(ConsumeTime, 10)])
        it "将棋データを更新" $ update ConsumeTime 10 shogi `shouldBe` Just newShogi
    describe "持将棋" $ do
      let newPosition = Position Black board stands
            $ Clocks (Clocks.Guillotine 7) (Clocks.Guillotine 10)
      let newShogi = Shogi
            (Draw Impasse)
            (Positions.cons newPosition (Positions.singleton position))
            (Updates [(CloseImpasse, 3)])
      it "将棋データを更新" $ update CloseImpasse 3 shogi `shouldBe` Just newShogi
    describe "千日手" $ do
      let board = Board.fromList
            [((F5, R9), Piece Black King), ((F5, R1), Piece White King)]
      let stands    = Stands.fromTuple ([(Gold, 1)], [(Gold, 1)])
      let position  = Position Black board stands $ Clocks.guillotine 10
      let positions = Positions.singleton position
      let shogi     = Shogi Open positions Updates.empty
      let moves =
            [ Move (F5, R9) False (F5, R8)
            , Move (F5, R1) False (F5, R2)
            , Move (F5, R8) False (F5, R9)
            , Move (F5, R2) False (F5, R1)
            , Move (F5, R9) False (F5, R8)
            , Move (F5, R1) False (F5, R2)
            , Move (F5, R8) False (F5, R9)
            , Move (F5, R2) False (F5, R1)
            , Move (F5, R9) False (F5, R8)
            , Move (F5, R1) False (F5, R2)
            , Move (F5, R8) False (F5, R9)
            , Move (F5, R2) False (F5, R1)
            ]
      let newShogi = foldl' (\s u -> s >>= update u 1) (Just shogi) moves
      it "将棋データを更新" $ do
        isJust newShogi `shouldBe` True
        length . unPositions . shogiPositions <$> newShogi `shouldBe` pure
          (length moves + 1)
        shogiStatus <$> newShogi `shouldBe` pure (Draw Repetition)
        shogiUpdates <$> newShogi `shouldBe` pure
          (Updates
            (reverse
              [ (Move (F5, R9) False (F5, R8), 1)
              , (Move (F5, R1) False (F5, R2), 1)
              , (Move (F5, R8) False (F5, R9), 1)
              , (Move (F5, R2) False (F5, R1), 1)
              , (Move (F5, R9) False (F5, R8), 1)
              , (Move (F5, R1) False (F5, R2), 1)
              , (Move (F5, R8) False (F5, R9), 1)
              , (Move (F5, R2) False (F5, R1), 1)
              , (Move (F5, R9) False (F5, R8), 1)
              , (Move (F5, R1) False (F5, R2), 1)
              , (Move (F5, R8) False (F5, R9), 1)
              , (Move (F5, R2) False (F5, R1), 1)
              ]
            )
          )
    describe "連続王手の千日手" $ do
      let board = Board.fromList
            [((F5, R9), Piece Black King), ((F5, R1), Piece White King)]
      let stands    = Stands.fromTuple ([(Rook, 1), (Gold, 1)], [(Gold, 1)])
      let position  = Position Black board stands $ Clocks.guillotine 10
      let positions = Positions.singleton position
      let shogi     = Shogi Open positions Updates.empty
      let moves =
            [ Drop Rook (F5, R5)
            , Move (F5, R1) False (F4, R1)
            , Move (F5, R5) False (F4, R5)
            , Move (F4, R1) False (F5, R1)
            , Move (F4, R5) False (F5, R5)
            , Move (F5, R1) False (F4, R1)
            , Move (F5, R5) False (F4, R5)
            , Move (F4, R1) False (F5, R1)
            , Move (F4, R5) False (F5, R5)
            , Move (F5, R1) False (F4, R1)
            , Move (F5, R5) False (F4, R5)
            , Move (F4, R1) False (F5, R1)
            , Move (F4, R5) False (F5, R5)
            ]
      let newShogi = foldl' (\s u -> s >>= update u 1) (Just shogi) moves
      it "将棋データを更新" $ do
        isJust newShogi `shouldBe` True
        length . unPositions . shogiPositions <$> newShogi `shouldBe` pure
          (length moves + 1)
        shogiStatus <$> newShogi `shouldBe` pure
          (Closed White (Illegal PerpetualCheck))
        shogiUpdates <$> newShogi `shouldBe` pure
          (Updates
            (reverse
              [ (Drop Rook (F5, R5)          , 1)
              , (Move (F5, R1) False (F4, R1), 1)
              , (Move (F5, R5) False (F4, R5), 1)
              , (Move (F4, R1) False (F5, R1), 1)
              , (Move (F4, R5) False (F5, R5), 1)
              , (Move (F5, R1) False (F4, R1), 1)
              , (Move (F5, R5) False (F4, R5), 1)
              , (Move (F4, R1) False (F5, R1), 1)
              , (Move (F4, R5) False (F5, R5), 1)
              , (Move (F5, R1) False (F4, R1), 1)
              , (Move (F5, R5) False (F4, R5), 1)
              , (Move (F4, R1) False (F5, R1), 1)
              , (Move (F4, R5) False (F5, R5), 1)
              ]
            )
          )
  describe "movables" $ it "連続王手の千日手を返さない" $ do
    let board = Board.fromList
          [((F5, R9), Piece Black King), ((F5, R1), Piece White King)]
    let stands    = Stands.fromTuple ([(Rook, 1), (Gold, 1)], [(Gold, 1)])
    let position  = Position Black board stands $ Clocks.guillotine 10
    let positions = Positions.singleton position
    let initShogi = Shogi Open positions Updates.empty
    let moves =
          [ Drop Rook (F5, R5)
          , Move (F5, R1) False (F4, R1)
          , Move (F5, R5) False (F4, R5)
          , Move (F4, R1) False (F5, R1)
          , Move (F4, R5) False (F5, R5)
          , Move (F5, R1) False (F4, R1)
          , Move (F5, R5) False (F4, R5)
          , Move (F4, R1) False (F5, R1)
          , Move (F4, R5) False (F5, R5)
          , Move (F5, R1) False (F4, R1)
          , Move (F5, R5) False (F4, R5)
          , Move (F4, R1) False (F5, R1)
          ]
    let shogi = foldl' (\s u -> s >>= update u 1) (Just initShogi) moves
    let ms = Movables.fromList
          [ ( (F5, R9)
            , [ ((F6, R8), No)
              , ((F6, R9), No)
              , ((F5, R8), No)
              , ((F4, R8), No)
              , ((F4, R9), No)
              ]
            )
          , ( (F4, R5)
            , [ ((F9, R5), No)
              , ((F8, R5), No)
              , ((F7, R5), No)
              , ((F6, R5), No)
              , ((F4, R1), Option)
              , ((F4, R2), Option)
              , ((F4, R3), Option)
              , ((F4, R4), No)
              , ((F4, R6), No)
              , ((F4, R7), No)
              , ((F4, R8), No)
              , ((F4, R9), No)
              , ((F3, R5), No)
              , ((F2, R5), No)
              , ((F1, R5), No)
              ]
            )
          ]
    movables <$> shogi `shouldBe` Just ms
  describe "droppables" $ it "打ち歩詰めを返さない" $ do
    let board = Board.fromList
          [ ((F2, R1), Piece Black PromotedBishop)
          , ((F2, R5), Piece Black Silver)
          , ((F2, R3), Piece White Pawn)
          , ((F1, R3), Piece White King)
          ]
    let stands    = Stands.fromTuple ([(Pawn, 1)], [])
    let position  = Position Black board stands Clocks.infinity
    let positions = Positions.singleton position
    let shogi     = Shogi Open positions Updates.empty
    let nds = Set.fromList [(F2, R1), (F2, R5), (F2, R3), (F1, R3), (F1, R4)]
    let ds = Droppables.fromList
          [ ( Pawn
            , [ (file, rank)
              | file <- [F9 .. F1]
              , rank <- [R2 .. R9]
              , not $ Set.member (file, rank) nds
              ]
            )
          ]
    droppables shogi `shouldBe` ds

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
import qualified Prelude

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
              $ Clocks (Clocks.Countdown 7 0) (Clocks.Countdown 10 0)
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
              $ Clocks (Clocks.Countdown 7 0) (Clocks.Countdown 10 0)
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
              $ Clocks (Clocks.Countdown 7 0) (Clocks.Countdown 10 0)
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
              $ Clocks (Clocks.Countdown 7 0) (Clocks.Countdown 10 0)
        let newShogi = Shogi
              (Closed Black Mate)
              (Positions.cons newPosition (Positions.singleton position))
              (Updates [(Drop Gold (F5, R2), 3)])
        it "将棋データを更新"
          $          update (Drop Gold (F5, R2)) 3 shogi
          `shouldBe` Just newShogi
    describe "時間切れ" $ do
      let newPosition = Position Black board stands
            $ Clocks Clocks.Timeout (Clocks.Countdown 10 0)
      let newShogi = Shogi
            (Closed White ShogiX.Shogi.Timeout)
            (Positions.cons newPosition (Positions.singleton position))
            (Updates [(Move (F4, R3) True (F5, R2), 10)])
      it "将棋データを更新"
        $          update (Move (F4, R3) True (F5, R2)) 10 shogi
        `shouldBe` Just newShogi
    describe "投了" $ do
      let newPosition = Position Black board stands
            $ Clocks (Clocks.Countdown 7 0) (Clocks.Countdown 10 0)
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
              $ Clocks Clocks.Timeout (Clocks.Countdown 10 0)
        let newShogi = Shogi
              (Closed White ShogiX.Shogi.Timeout)
              (Positions.cons newPosition (Positions.singleton position))
              (Updates [(ConsumeTime, 10)])
        it "将棋データを更新" $ update ConsumeTime 10 shogi `shouldBe` Just newShogi
    describe "持将棋" $ do
      let newPosition = Position Black board stands
            $ Clocks (Clocks.Countdown 7 0) (Clocks.Countdown 10 0)
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

spec_Test_Kifu :: Spec
spec_Test_Kifu = describe "棋譜を再現した場合" $ it "将棋を返す" $ do
  let updates =
        [ Move (F7, R7) False (F7, R6)
        , Move (F3, R3) False (F3, R4)
        , Move (F2, R7) False (F2, R6)
        , Move (F4, R3) False (F4, R4)
        , Move (F3, R9) False (F4, R8)
        , Move (F3, R1) False (F3, R2)
        , Move (F5, R7) False (F5, R6)
        , Move (F5, R3) False (F5, R4)
        , Move (F4, R7) False (F4, R6)
        , Move (F3, R2) False (F4, R3)
        , Move (F3, R7) False (F3, R6)
        , Move (F8, R2) False (F4, R2)
        , Move (F4, R8) False (F4, R7)
        , Move (F2, R2) False (F3, R3)
        , Move (F5, R9) False (F6, R8)
        , Move (F5, R1) False (F6, R2)
        , Move (F6, R8) False (F7, R8)
        , Move (F6, R2) False (F7, R2)
        , Move (F4, R9) False (F5, R8)
        , Move (F7, R1) False (F6, R2)
        , Move (F1, R7) False (F1, R6)
        , Move (F1, R3) False (F1, R4)
        , Move (F9, R7) False (F9, R6)
        , Move (F9, R3) False (F9, R4)
        , Move (F7, R9) False (F6, R8)
        , Move (F6, R3) False (F6, R4)
        , Move (F6, R7) False (F6, R6)
        , Move (F7, R3) False (F7, R4)
        , Move (F2, R6) False (F2, R5)
        , Move (F4, R1) False (F5, R2)
        , Move (F5, R8) False (F6, R7)
        , Move (F5, R2) False (F6, R3)
        , Move (F6, R8) False (F7, R7)
        , Move (F7, R2) False (F8, R2)
        , Move (F2, R8) False (F3, R8)
        , Move (F6, R1) False (F7, R2)
        , Move (F3, R6) False (F3, R5)
        , Move (F3, R4) False (F3, R5)
        , Move (F3, R8) False (F3, R5)
        , Move (F4, R2) False (F3, R2)
        , Move (F3, R5) False (F3, R8)
        , Move (F8, R3) False (F8, R4)
        , Drop Pawn (F3, R5)
        , Move (F8, R4) False (F8, R5)
        , Move (F8, R8) False (F7, R9)
        , Move (F6, R2) False (F7, R3)
        , Move (F7, R8) False (F8, R8)
        , Move (F7, R3) False (F8, R4)
        , Move (F6, R9) False (F7, R8)
        , Move (F3, R3) False (F5, R1)
        , Move (F4, R7) False (F3, R6)
        , Move (F9, R4) False (F9, R5)
        , Move (F9, R6) False (F9, R5)
        , Move (F8, R4) False (F9, R5)
        , Drop Pawn (F9, R7)
        , Move (F7, R4) False (F7, R5)
        , Move (F7, R6) False (F7, R5)
        , Drop Pawn (F7, R4)
        , Move (F7, R5) False (F7, R4)
        , Drop Pawn (F7, R5)
        , Move (F4, R6) False (F4, R5)
        , Move (F6, R3) False (F7, R4)
        , Move (F7, R9) False (F4, R6)
        , Move (F8, R1) False (F7, R3)
        , Drop Pawn (F7, R6)
        , Move (F7, R5) False (F7, R6)
        , Move (F7, R7) False (F7, R6)
        , Drop Pawn (F7, R5)
        , Move (F7, R6) False (F7, R5)
        , Move (F7, R4) False (F7, R5)
        , Move (F4, R6) False (F6, R4)
        , Move (F7, R5) False (F7, R4)
        , Move (F6, R4) True  (F5, R3)
        , Move (F4, R3) False (F5, R2)
        , Move (F5, R3) False (F5, R4)
        , Drop Silver (F4, R3)
        , Move (F5, R4) False (F5, R5)
        , Drop Pawn (F5, R4)
        , Move (F5, R5) False (F4, R6)
        , Move (F8, R5) False (F8, R6)
        , Move (F9, R7) False (F9, R6)
        , Move (F8, R6) True  (F8, R7)
        , Move (F7, R8) False (F8, R7)
        , Move (F9, R5) False (F8, R4)
        , Drop Pawn (F8, R5)
        , Move (F8, R4) False (F8, R5)
        , Drop Pawn (F8, R6)
        , Move (F8, R5) False (F9, R4)
        , Move (F3, R8) False (F7, R8)
        , Drop Pawn (F7, R5)
        , Drop Pawn (F7, R6)
        , Move (F7, R5) False (F7, R6)
        , Move (F6, R7) False (F7, R6)
        , Drop Pawn (F7, R5)
        , Move (F7, R6) False (F7, R5)
        , Move (F7, R4) False (F7, R5)
        , Move (F7, R8) False (F7, R5)
        , Drop Gold (F8, R4)
        , Move (F7, R5) False (F7, R8)
        , Drop Pawn (F7, R4)
        , Move (F9, R6) False (F9, R5)
        , Move (F9, R4) False (F8, R3)
        , Move (F8, R9) False (F7, R7)
        , Move (F8, R2) False (F8, R1)
        , Drop Gold (F6, R7)
        , Move (F5, R1) False (F3, R3)
        , Move (F8, R6) False (F8, R5)
        , Move (F8, R4) False (F7, R5)
        , Drop Pawn (F7, R6)
        , Move (F4, R4) False (F4, R5)
        , Move (F4, R6) False (F5, R7)
        , Drop Pawn (F8, R6)
        , Move (F8, R7) False (F9, R6)
        , Move (F9, R1) False (F9, R5)
        , Move (F9, R6) False (F9, R5)
        , Move (F7, R3) False (F6, R5)
        , Move (F6, R6) False (F6, R5)
        , Drop Pawn (F6, R6)
        , Move (F7, R6) False (F7, R5)
        , Move (F6, R6) True  (F6, R7)
        , Move (F5, R7) False (F6, R7)
        , Drop Gold (F8, R7)
        , Move (F8, R8) False (F7, R9)
        , Move (F8, R7) False (F7, R8)
        , Move (F7, R9) False (F7, R8)
        , Drop Rook   (F3, R8)
        , Drop Knight (F5, R8)
        , Move (F7, R4) False (F7, R5)
        , Move (F8, R5) False (F8, R4)
        , Drop Pawn (F6, R6)
        , Move (F6, R7) False (F8, R5)
        , Move (F8, R3) False (F7, R4)
        , Drop Lance (F6, R4)
        , Move (F5, R2) False (F6, R3)
        , Move (F6, R4) False (F6, R3)
        , Move (F7, R2) False (F6, R3)
        , Move (F3, R6) False (F4, R7)
        , Move (F3, R8) True  (F3, R9)
        , Move (F8, R4) True  (F8, R3)
        , Move (F7, R4) False (F8, R5)
        , Move (F9, R5) False (F8, R5)
        , Move (F3, R9) False (F9, R9)
        , Drop Silver (F6, R1)
        , Move (F6, R6) True  (F6, R7)
        , Move (F7, R8) False (F6, R7)
        , Move (F9, R9) False (F6, R9)
        , Drop Gold  (F6, R8)
        , Drop Lance (F6, R6)
        , Move (F5, R8) False (F6, R6)
        , Drop Bishop (F7, R8)
        , Move (F6, R7) False (F5, R7)
        , Move (F3, R3) False (F6, R6)
        , Move (F5, R7) False (F6, R6)
        , Move (F6, R9) False (F6, R8)
        , Move (F6, R6) False (F7, R5)
        , Move (F6, R8) False (F7, R7)
        , Drop Pawn (F7, R6)
        , Drop Gold (F7, R4)
        , Move (F8, R5) False (F7, R4)
        , Move (F6, R3) False (F7, R4)
        , Move (F7, R5) False (F7, R4)
        , Move (F7, R7) False (F7, R6)
        , Drop Gold (F7, R5)
        , Move (F7, R8) True  (F9, R6)
        , Move (F7, R4) False (F8, R4)
        , Drop Gold (F7, R4)
        , Move (F8, R4) False (F9, R3)
        , Drop Lance (F9, R2)
        , Move (F8, R3) False (F9, R2)
        , Move (F3, R2) False (F9, R2)
        , CloseResign
        ]
  let shogi = foldl' (\s u -> s >>= update u 0)
                     (Just (hirate Clocks.infinity))
                     updates
      position = shogiPosition <$> shogi
      turn     = positionTurn <$> position
      board    = positionBoard <$> position
      stands   = positionStands <$> position
  shogiStatus <$> shogi `shouldBe` Just (Closed White Resign)
  turn `shouldBe` Just Black
  board `shouldBe` Just
    (Board.fromList
      [ ((F8, R1), Piece White King)
      , ((F6, R1), Piece Black Silver)
      , ((F2, R1), Piece White Knight)
      , ((F1, R1), Piece White Lance)
      , ((F9, R2), Piece White Rook)
      , ((F9, R3), Piece Black King)
      , ((F4, R3), Piece White Silver)
      , ((F2, R3), Piece White Pawn)
      , ((F7, R4), Piece White Gold)
      , ((F5, R4), Piece White Pawn)
      , ((F1, R4), Piece White Pawn)
      , ((F7, R5), Piece Black Gold)
      , ((F6, R5), Piece Black Pawn)
      , ((F4, R5), Piece White Pawn)
      , ((F3, R5), Piece Black Pawn)
      , ((F2, R5), Piece Black Pawn)
      , ((F9, R6), Piece White PromotedBishop)
      , ((F8, R6), Piece White Pawn)
      , ((F7, R6), Piece White PromotedRook)
      , ((F5, R6), Piece Black Pawn)
      , ((F1, R6), Piece Black Pawn)
      , ((F4, R7), Piece Black Silver)
      , ((F2, R9), Piece Black Knight)
      , ((F1, R9), Piece Black Lance)
      ]
    )
  stands `shouldBe` Just
    (Stands.fromTuple
      ( [(Bishop, 1), (Gold, 2), (Silver, 1), (Lance, 2), (Pawn, 6)]
      , [(Knight, 2), (Pawn, 2)]
      )
    )


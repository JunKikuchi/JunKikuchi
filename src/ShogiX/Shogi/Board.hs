module ShogiX.Shogi.Board
  ( empty
  , hirate
  , fromList
  , move
  , ShogiX.Shogi.Board.drop
  , checked
  , movables
  , kingSquares
  )
where

import           RIO
import qualified RIO.Map                       as Map
import qualified RIO.Set                       as Set
import           ShogiX.Shogi.Color             ( turnColor )
import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Piece            as Piece
import qualified ShogiX.Shogi.Movables         as Movables

-- | 空の将棋盤
empty :: Board
empty = Board Map.empty

-- | 平手の将棋盤
hirate :: Board
hirate = fromList
  [ ((F9, R1), Piece White Lance)
  , ((F8, R1), Piece White Knight)
  , ((F7, R1), Piece White Silver)
  , ((F6, R1), Piece White Gold)
  , ((F5, R1), Piece White King)
  , ((F4, R1), Piece White Gold)
  , ((F3, R1), Piece White Silver)
  , ((F2, R1), Piece White Knight)
  , ((F1, R1), Piece White Lance)
  , ((F8, R2), Piece White Rook)
  , ((F2, R2), Piece White Bishop)
  , ((F9, R3), Piece White Pawn)
  , ((F8, R3), Piece White Pawn)
  , ((F7, R3), Piece White Pawn)
  , ((F6, R3), Piece White Pawn)
  , ((F5, R3), Piece White Pawn)
  , ((F4, R3), Piece White Pawn)
  , ((F3, R3), Piece White Pawn)
  , ((F2, R3), Piece White Pawn)
  , ((F1, R3), Piece White Pawn)
  , ((F9, R7), Piece Black Pawn)
  , ((F8, R7), Piece Black Pawn)
  , ((F7, R7), Piece Black Pawn)
  , ((F6, R7), Piece Black Pawn)
  , ((F5, R7), Piece Black Pawn)
  , ((F4, R7), Piece Black Pawn)
  , ((F3, R7), Piece Black Pawn)
  , ((F2, R7), Piece Black Pawn)
  , ((F1, R7), Piece Black Pawn)
  , ((F8, R8), Piece Black Bishop)
  , ((F2, R8), Piece Black Rook)
  , ((F9, R9), Piece Black Lance)
  , ((F8, R9), Piece Black Knight)
  , ((F7, R9), Piece Black Silver)
  , ((F6, R9), Piece Black Gold)
  , ((F5, R9), Piece Black King)
  , ((F4, R9), Piece Black Gold)
  , ((F3, R9), Piece Black Silver)
  , ((F2, R9), Piece Black Knight)
  , ((F1, R9), Piece Black Lance)
  ]

-- | リストから将棋盤を作成
fromList :: [(Square, Piece)] -> Board
fromList = Board . Map.fromList

-- | 駒の移動
move
  :: SrcSquare
  -> Promotion
  -> DestSquare
  -> Board
  -> Maybe (Board, Maybe PieceType)
move src promo dest board = do
  piece      <- Map.lookup src b
  promotable <- Map.lookup dest . unMovable . Piece.movable piece src $ ss
  guard
    (  (promotable == No && not promo)
    || (promotable == Option)
    || (promotable == Must && promo)
    )
  let deleted  = Map.delete src b
      p        = Piece.promote promo piece
      newBoard = Map.insert dest p deleted
  pure (Board newBoard, capture)
 where
  b       = unBoard board
  ss      = Map.map pieceColor b
  capture = pieceType <$> Map.lookup dest b

-- | 駒の打ち込み
drop :: Color -> PieceType -> DestSquare -> Board -> Maybe Board
drop _ PromotedPawn   _ _ = Nothing
drop _ PromotedLance  _ _ = Nothing
drop _ PromotedKnight _ _ = Nothing
drop _ PromotedSilver _ _ = Nothing
drop _ PromotedBishop _ _ = Nothing
drop _ PromotedRook   _ _ = Nothing
drop color pt dest board
  | Map.member dest b = Nothing
  | otherwise = do
    guard (Set.member dest . unDroppable $ droppable)
    pure (Board (Map.insert dest (Piece color pt) b))
 where
  droppable = Piece.droppable color pt board
  b         = unBoard board

-- | 王手判定
checked :: Color -> Board -> Bool
checked color board = ks /= Set.empty && Set.intersection ms ks == ks
 where
  ms = Movables.destSquareSet $ movables (turnColor color) board
  ks = kingSquares color board

-- | 玉のマス目取得
kingSquares :: Color -> Board -> Set Square
kingSquares = pieceSquares King

-- | 駒のマス目取得
pieceSquares :: PieceType -> Color -> Board -> Set Square
pieceSquares pt color board = Map.keysSet $ Map.filter
  (\p -> pieceColor p == color && pieceType p == pt)
  (unBoard board)

-- | 駒の移動範囲を取得
movables :: Color -> Board -> Movables
movables color board = Movables $ Map.foldrWithKey build Map.empty b
 where
  b  = unBoard board
  ss = Map.map pieceColor b
  build square piece acc | sameColor && hasMovable = Map.insert square m acc
                         | otherwise               = acc
   where
    sameColor  = pieceColor piece == color
    hasMovable = not . Map.null . unMovable $ m
    m          = Piece.movable piece square ss

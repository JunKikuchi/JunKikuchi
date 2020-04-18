module ShogiX.Shogi.Board
  ( empty
  , fromList
  , move
  , ShogiX.Shogi.Board.drop
  , checked
  , movables
  )
where

import           RIO
import qualified RIO.Map                       as Map
import qualified RIO.Set                       as Set
import           ShogiX.Shogi.Color             ( turnColor )
import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Piece            as Piece

-- | 空の将棋盤
empty :: Board
empty = Board Map.empty

-- | リストから将棋盤を作成
fromList :: [(Square, Piece)] -> Board
fromList = Board . Map.fromList

-- | 駒の移動
move
  :: SrcSquare
  -> Promotion
  -> DestSquare
  -> Board
  -> Either CloseStatus (Board, Maybe PieceType)
move src promo dest board = do
  piece      <- f (Map.lookup src b)
  promotable <- f (Map.lookup dest . unMovable . Piece.movable piece src $ ss)
  g promotable
  let deleted  = Map.delete src b
      p        = Piece.promote promo piece
      newBoard = Map.insert dest p deleted
  pure (Board newBoard, capture)
 where
  f = maybe e pure
  g p =
    unless ((p == No && not promo) || (p == Option) || (p == Must && promo)) e
  e       = Left (Illegal IllegalMove)
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
  ms = Set.unions $ (Set.fromList . Map.keys . unMovable) <$> Map.elems
    (unMovables $ movables (turnColor color) board)
  ks = kingSquares color board

{--
-- | 玉の可動範囲
-- let kings = kingSquares Color ...
-- let ms  = kingMovableSquares Color ... kings
kingMovableSquares :: Color -> Board -> Set Square -> Set Square
kingMovableSquares color board = Set.unions . Set.map fs
 where
  fs =
    Set.fromList
      . Map.keys
      . unMovable
      . flip (Piece.movable (Piece color King)) ss
  ss = Map.map pieceColor (unBoard board)
--}

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
  build square piece acc
    | pc == color && not emptyMovable = Map.insert square m acc
    | otherwise                       = acc
   where
    pc           = pieceColor piece
    emptyMovable = Map.null $ unMovable m
    m            = Piece.movable piece square ss

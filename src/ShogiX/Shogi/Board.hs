module ShogiX.Shogi.Board where

import           RIO
import qualified RIO.Map                       as Map
import qualified RIO.Set                       as Set
import           ShogiX.Shogi.Color             ( turnColor )
import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Piece            as Piece

-- | 空の将棋盤
empty :: Board
empty = Board Map.empty

-- | 駒の移動
move
  :: SrcSquare
  -> Promotion
  -> DestSquare
  -> Board
  -> Maybe (Board, Maybe PieceType)
move = undefined

-- | 王手判定
checked :: Color -> Board -> Bool
checked color board = Set.intersection ms ks == ks
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

module ShogiX.Shogi.Board where

import           RIO
import qualified RIO.Map                       as Map
import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Piece            as Piece

-- | 空の将棋盤
empty :: Board
empty = Board Map.empty

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

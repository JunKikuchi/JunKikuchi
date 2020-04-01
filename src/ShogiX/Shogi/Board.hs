module ShogiX.Shogi.Board where

import           RIO
import qualified RIO.Map                       as Map
import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Piece            as Piece

-- | 駒の移動範囲を取得
movables :: Color -> Board -> Movables
movables color board = Movables $ Map.foldrWithKey build Map.empty b
 where
  b  = unBoard board
  ss = Map.map pieceColor b
  build square piece acc | pc == color && not empty = Map.insert square m acc
                         | otherwise                = acc
   where
    pc    = pieceColor piece
    empty = Map.null $ unMovable m
    m     = Piece.movable piece square ss

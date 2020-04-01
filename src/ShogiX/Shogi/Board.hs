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
  build square piece acc
    | pieceColor piece == color && not (Map.null m) = Map.insert square m acc
    | otherwise = acc
    where m = Piece.movable piece square ss

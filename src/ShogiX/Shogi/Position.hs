module ShogiX.Shogi.Position where

import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Board            as Board

-- | 駒の移動範囲を取得
movables :: Position -> Movables
movables pos = Board.movables turn board
 where
  turn  = positionTurn pos
  board = positionBoard pos

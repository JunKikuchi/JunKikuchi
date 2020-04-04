module ShogiX.Shogi.Position where

import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Board            as Board
import qualified ShogiX.Shogi.Stand            as Stand

-- | 駒の移動範囲を取得
movables :: Position -> Movables
movables pos = Board.movables turn board
 where
  turn  = positionTurn pos
  board = positionBoard pos

-- | 持ち駒の打ち先範囲を取得
droppables :: Position -> Droppables
droppables pos = Stand.droppables turn board stands
 where
  turn   = positionTurn pos
  board  = positionBoard pos
  stands = positionStands pos

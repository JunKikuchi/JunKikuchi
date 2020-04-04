module ShogiX.Shogi.Stand where

import           RIO
import qualified RIO.Map                       as Map
import qualified RIO.Set                       as Set
import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Piece            as Piece

-- | 持ち駒の打ち先範囲を取得
droppables :: Color -> Board -> Stands -> Droppables
droppables color board stands = Droppables (Map.fromList ds)
 where
  ds  = [ (Piece.droppable color pt b, Set.singleton pt) | pt <- pts ]
  b   = unBoard board
  pts = Map.keys . unStand $ stand color stands

-- | 先手後手の駒台から駒台取得
stand :: Color -> Stands -> Stand
stand Black = blackStand
stand White = whiteStand

-- | 空の駒台
empty :: Stand
empty = Stand Map.empty
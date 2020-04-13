module ShogiX.Shogi.Stands
  ( empty
  , fromList
  , stand
  , droppables
  )
where

import           RIO
import qualified RIO.Map                       as Map
import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Piece            as Piece
import qualified ShogiX.Shogi.Stand            as Stand

-- | 空の駒台
empty :: Stands
empty = Stands Stand.empty Stand.empty

-- | リストから駒台を作成
fromList :: [(PieceType, Int)] -> [(PieceType, Int)] -> Stands
fromList b w = Stands (Stand.fromList b) (Stand.fromList w)

-- | 先手後手の駒台から駒台取得
stand :: Color -> Stands -> Stand
stand Black = blackStand
stand White = whiteStand

-- | 持ち駒の打ち先範囲を取得
droppables :: Color -> Board -> Stands -> Droppables
droppables color board stands = Droppables (Map.fromList ds)
 where
  ds  = [ (pt, Piece.droppable color pt board) | pt <- pts ]
  pts = Map.keys . unStand $ stand color stands

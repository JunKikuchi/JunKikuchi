module ShogiX.Shogi.Stand
  ( empty
  , fromList
  , droppables
  )
where

import           RIO
import qualified RIO.Map                       as Map
import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Piece            as Piece

-- | 空の駒台
empty :: Stand
empty = Stand Map.empty

-- | リストから駒台を作成
fromList :: [(PieceType, Int)] -> Stand
fromList = Stand . Map.fromList

-- | 持ち駒の打ち先範囲を取得
droppables :: Color -> Board -> Stands -> Droppables
droppables color board stands = Droppables (Map.fromList ds)
 where
  ds  = [ (pt, Piece.droppable color pt board) | pt <- pts ]
  pts = Map.keys . unStand $ stand color stands

-- | 先手後手の駒台から駒台取得
stand :: Color -> Stands -> Stand
stand Black = blackStand
stand White = whiteStand

module ShogiX.Shogi.Droppables
  ( empty
  , fromList
  )
where

import           RIO
import qualified RIO.Map                       as Map
import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Droppable        as Droppable

-- | 空の駒の打ち先作成
empty :: Droppables
empty = Droppables Map.empty

-- | リストから持ち駒の打ち先を作成
fromList :: [(PieceType, [DestSquare])] -> Droppables
fromList = Droppables . Map.fromList . map (second Droppable.fromList)

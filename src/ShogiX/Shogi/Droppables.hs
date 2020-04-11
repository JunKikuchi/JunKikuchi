module ShogiX.Shogi.Droppables
  ( fromList
  )
where

import           RIO
import qualified RIO.Map                       as Map
import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Droppable        as Droppable

-- | リストから持ち駒の打ち先を作成
fromList :: [(PieceType, [DestSquare])] -> Droppables
fromList = Droppables . Map.fromList . map (second Droppable.fromList)

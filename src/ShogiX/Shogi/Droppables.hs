module ShogiX.Shogi.Droppables
  ( empty
  , fromList
  , ShogiX.Shogi.Droppables.null
  )
where

import           RIO
import qualified RIO.Map                       as Map
import qualified RIO.Set                       as Set
import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Droppable        as Droppable

-- | 空の駒の打ち先作成
empty :: Droppables
empty = Droppables Map.empty

-- | リストから持ち駒の打ち先を作成
fromList :: [(PieceType, [DestSquare])] -> Droppables
fromList = Droppables . Map.fromList . map (second Droppable.fromList)

-- | 打ち先の有無
null :: Droppables -> Bool
null = Set.null . Set.unions . fmap unDroppable . Map.elems . unDroppables

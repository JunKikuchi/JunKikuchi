module ShogiX.Shogi.Stands
  ( empty
  , fromList
  )
where

import           RIO
import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Stand            as Stand

-- | 空の駒台
empty :: Stands
empty = Stands Stand.empty Stand.empty

-- | リストから駒台を作成
fromList :: [(PieceType, Int)] -> [(PieceType, Int)] -> Stands
fromList b w = Stands (Stand.fromList b) (Stand.fromList w)

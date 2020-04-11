module ShogiX.Shogi.Stands
  ( empty
  )
where

import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Stand            as Stand

-- | 空の駒台
empty :: Stands
empty = Stands Stand.empty Stand.empty

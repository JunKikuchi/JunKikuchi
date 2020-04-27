module ShogiX.Shogi.Updates
  ( empty
  , cons
  )
where

import           RIO
import           ShogiX.Shogi.Types
import           ShogiX.Clocks                  ( Sec )

-- | 空の更新履歴
empty :: Updates
empty = Updates []

-- | 更新履歴を追加
cons :: (Update, Sec) -> Updates -> Updates
cons u = Updates . (u :) . unUpdates

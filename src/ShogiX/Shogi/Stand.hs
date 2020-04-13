module ShogiX.Shogi.Stand
  ( empty
  , fromList
  )
where

import           RIO
import qualified RIO.Map                       as Map
import           ShogiX.Shogi.Types

-- | 空の駒台
empty :: Stand
empty = Stand Map.empty

-- | リストから駒台を作成
fromList :: [(PieceType, Int)] -> Stand
fromList = Stand . Map.fromList

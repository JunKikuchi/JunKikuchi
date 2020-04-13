module ShogiX.Shogi.Stand
  ( empty
  , fromList
  , add
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

-- |
add :: PieceType -> Stand -> Stand
add pt stand | Map.member pt s = Stand (Map.update (pure . (+ 1)) pt s)
             | otherwise       = Stand (Map.insert pt 1 s)
  where s = unStand stand

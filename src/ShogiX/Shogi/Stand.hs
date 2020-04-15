module ShogiX.Shogi.Stand
  ( empty
  , fromList
  , add
  , ShogiX.Shogi.Stand.drop
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

-- | 駒台に駒を追加
-- >>> add Pawn empty
-- Stand {unStand = fromList [(Pawn,1)]}
--
-- >>> add Pawn (fromList [(Pawn, 1)])
-- Stand {unStand = fromList [(Pawn,2)]}
add :: PieceType -> Stand -> Stand
add pt stand | Map.member pt s = Stand (Map.update (pure . (+ 1)) pt s)
             | otherwise       = Stand (Map.insert pt 1 s)
  where s = unStand stand

-- | 駒台の駒を削除
-- >>> ShogiX.Shogi.Stand.drop Pawn (fromList [(Pawn, 2)])
-- Just (Stand {unStand = fromList [(Pawn,1)]})
--
-- >>> ShogiX.Shogi.Stand.drop Pawn (fromList [(Pawn, 1)])
-- Just (Stand {unStand = fromList []})
drop :: PieceType -> Stand -> Maybe Stand
drop pt stand | Map.member pt s = Just (Stand newStand)
              | otherwise       = Nothing
 where
  s        = unStand stand
  newStand = Map.update f pt s
  f n = if n <= 1 then Nothing else pure (n - 1)

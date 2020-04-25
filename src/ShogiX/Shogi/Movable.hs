module ShogiX.Shogi.Movable
  ( empty
  , fromList
  )
where

import           RIO
import qualified RIO.Map                       as Map
import           ShogiX.Shogi.Types

-- | 空の移動先
empty :: Movable
empty = fromList []

-- | リストから駒の移動先を作成
fromList :: [(DestSquare, Promotable)] -> Movable
fromList = Movable . Map.fromList

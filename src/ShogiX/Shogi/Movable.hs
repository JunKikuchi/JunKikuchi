module ShogiX.Shogi.Movable
  ( fromList
  )
where

import           RIO
import qualified RIO.Map                       as Map
import           ShogiX.Shogi.Types

-- | リストから駒の移動先を作成
fromList :: [(DestSquare, Promotable)] -> Movable
fromList = Movable . Map.fromList

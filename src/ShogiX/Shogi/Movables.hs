module ShogiX.Shogi.Movables
  ( fromList
  )
where

import           RIO
import qualified RIO.Map                       as Map
import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Movable          as Movable

-- | リストから駒の移動先を作成
fromList :: [(SrcSquare, [(DestSquare, Promotable)])] -> Movables
fromList = Movables . Map.fromList . map (second Movable.fromList)

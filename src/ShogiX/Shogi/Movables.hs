module ShogiX.Shogi.Movables
  ( empty
  , fromList
  )
where

import           RIO
import qualified RIO.Map                       as Map
import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Movable          as Movable

-- | 空の駒の打ち先作成
empty :: Movables
empty = Movables Map.empty

-- | リストから駒の移動先を作成
fromList :: [(SrcSquare, [(DestSquare, Promotable)])] -> Movables
fromList = Movables . Map.fromList . map (second Movable.fromList)

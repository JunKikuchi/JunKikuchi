module ShogiX.Shogi.Movables
  ( empty
  , fromList
  , ShogiX.Shogi.Movables.null
  , destSquareSet
  )
where

import           RIO
import qualified RIO.Map                       as Map
import qualified RIO.Set                       as Set
import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Movable          as Movable

-- | 空の駒の打ち先作成
empty :: Movables
empty = Movables Map.empty

-- | リストから駒の移動先を作成
fromList :: [(SrcSquare, [(DestSquare, Promotable)])] -> Movables
fromList = Movables . Map.fromList . map (second Movable.fromList)

-- | 移動先の有無
null :: Movables -> Bool
null = Map.null . unMovables

-- | 駒の移動先マス目セット作成
destSquareSet :: Movables -> Set DestSquare
destSquareSet =
  Set.unions
    . fmap (Set.fromList . Map.keys . unMovable)
    . Map.elems
    . unMovables

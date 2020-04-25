module ShogiX.Shogi.Droppable
  ( empty
  , fromList
  )
where

import           RIO
import qualified RIO.Set                       as Set
import           ShogiX.Shogi.Types

-- | 空の打ち先
empty :: Droppable
empty = fromList []

-- | リストから持ち駒の打ち先を作成
fromList :: [DestSquare] -> Droppable
fromList = Droppable . Set.fromList

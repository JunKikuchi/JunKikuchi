module ShogiX.Shogi.Droppable
  ( fromList
  )
where

import           RIO
import qualified RIO.Set                       as Set
import           ShogiX.Shogi.Types

-- | リストから持ち駒の打ち先を作成
fromList :: [DestSquare] -> Droppable
fromList = Droppable . Set.fromList

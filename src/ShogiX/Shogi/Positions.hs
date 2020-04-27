module ShogiX.Shogi.Positions
  ( hirate
  , ShogiX.Shogi.Positions.head
  , ShogiX.Shogi.Positions.take
  , ShogiX.Shogi.Positions.filter
  , cons
  )
where

import           RIO
import qualified RIO.NonEmpty                  as NE
import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Position         as Position
import           ShogiX.Clocks                  ( Clocks )

-- | 平手の局面
hirate :: Clocks -> Positions
hirate clocks = Positions (Position.hirate clocks :| [])

-- | 先頭の局面取得
head :: Positions -> Position
head = NE.head . unPositions

-- | 局面リストの先頭を取得
take :: Int -> Positions -> [Position]
take n = NE.take n . unPositions

-- | 同じ局面のリストを取得
filter :: Position -> Positions -> [Position]
filter pos = NE.filter (Position.positionEq pos) . unPositions

-- | 局面を追加
cons :: Position -> Positions -> Positions
cons pos = Positions . (pos NE.<|) . unPositions

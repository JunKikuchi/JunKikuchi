module ShogiX.Shogi.Positions
  ( hirate
  , singleton
  , cons
  , ShogiX.Shogi.Positions.head
  , ShogiX.Shogi.Positions.take
  , ShogiX.Shogi.Positions.filter
  )
where

import           RIO
import qualified RIO.NonEmpty                  as NE
import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Position         as Position
import           ShogiX.Clocks                  ( Clocks )

-- | 平手の局面
hirate :: Clocks -> Positions
hirate clocks = singleton $ Position.hirate clocks

-- | 局面履歴作成
singleton :: Position -> Positions
singleton = Positions . (:| [])

-- | 局面を追加
cons :: Position -> Positions -> Positions
cons pos = Positions . (pos NE.<|) . unPositions

-- | 先頭の局面取得
head :: Positions -> Position
head = NE.head . unPositions

-- | 局面履歴の先頭を取得
take :: Int -> Positions -> [Position]
take n = NE.take n . unPositions

-- | 同じ局面のリストを取得
filter :: Position -> Positions -> [Position]
filter pos = NE.filter (Position.positionEq pos) . unPositions

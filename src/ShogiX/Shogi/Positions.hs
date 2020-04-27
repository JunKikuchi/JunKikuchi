module ShogiX.Shogi.Positions where

import           RIO
import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Position         as Position
import           ShogiX.Clocks                  ( Clocks )

-- | 平手の局面
hirate :: Clocks -> Positions
hirate clocks = Positions (Position.hirate clocks :| [])
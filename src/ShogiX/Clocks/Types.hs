module ShogiX.Clocks.Types where

import           RIO

-- | 対局時計
data Clocks
  = Clocks
  { blackClock :: Clock
  , whiteClock :: Clock
  } deriving (Show, Eq)

-- | 持ち時間
data Clock
  = Infinity                            -- ^ 無制限
  | Countdown AllotmentSec CountdownSec -- ^ 秒読み (持ち時間 秒読み)
  | Fischer AllotmentSec IncrementSec   -- ^ フィッシャーモード (持ち時間 加算時間)
  | Timeout                             -- ^ 時間切れ
  deriving (Show, Eq)

-- | 秒
type Sec = Int

-- | 持ち時間
type AllotmentSec = Sec

-- | 秒読み時間
type CountdownSec = Sec

-- | 加算時間
type IncrementSec = Sec

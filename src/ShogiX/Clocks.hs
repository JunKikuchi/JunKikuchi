module ShogiX.Clocks where

-- | 対局時計
data Clocks
  = Clocks
  { blackClock :: Unit
  , whiteClock :: Unit
  } deriving Show

-- | 持ち時間設定
data Unit
  = Infinity          -- ^ 無制限
  | Guillotine Sec    -- ^ 差し切り
  | Countdown Sec Sec -- ^ 秒読み (持ち時間 秒読み)
  | Fischer Sec Sec   -- ^ フィッシャーモード (持ち時間 加算時間)
  deriving Show

type Sec = Int

-- | 無制限
infinity :: Clocks
infinity = Clocks Infinity Infinity

-- | 差し切り
guillotine :: Sec -> Clocks
guillotine sec = Clocks (Guillotine sec) (Guillotine sec)

-- | 秒読み
countdown :: Sec -> Sec -> Clocks
countdown allotment sec =
  Clocks (Countdown allotment sec) (Countdown allotment sec)

-- | フィッシャーモード
fischer :: Sec -> Sec -> Clocks
fischer allotment sec = Clocks (Fischer allotment sec) (Fischer allotment sec)

module ShogiX.Clocks where

import           ShogiX.Clocks.Types
import           ShogiX.Shogi.Types             ( Color(..) )

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

-- | 持ち時間消費
--
-- >>> let clocks = infinity
-- >>> consume 10 Black clocks
-- Clocks {blackClock = Infinity, whiteClock = Infinity}
--
-- >>> let clocks = guillotine 60
-- >>> consume 10 Black clocks
-- Clocks {blackClock = Guillotine 50, whiteClock = Guillotine 60}
consume :: Sec -> Color -> Clocks -> Clocks
consume sec color clocks = newClocks
 where
  colorClock    = getClock color clocks
  consumedClock = consumeClockSec sec colorClock
  newClocks     = setClock color consumedClock clocks

-- | 手番の時計取得
--
-- >>> let clocks = Clocks Infinity (Guillotine 60)
-- >>> getClock Black clocks
-- Infinity
getClock :: Color -> Clocks -> Clock
getClock Black = blackClock
getClock White = whiteClock

-- | 手番の時計設定
--
-- >>> let clocks = Clocks (Guillotine 60) (Guillotine 60)
-- >>> setClock Black (Guillotine 50) clocks
-- Clocks {blackClock = Guillotine 50, whiteClock = Guillotine 60}
setClock :: Color -> Clock -> Clocks -> Clocks
setClock Black clock clocks = clocks { blackClock = clock }
setClock White clock clocks = clocks { whiteClock = clock }

-- | 時計の時間を消費
--
-- >>> consumeClockSec 10 (Guillotine 60)
-- Guillotine 50
--
-- >>> consumeClockSec 65 (Guillotine 60)
-- Timeout
--
-- >>> consumeClockSec 10 (Countdown 60 10)
-- Countdown 50 10
--
-- >>> consumeClockSec 65 (Countdown 60 10)
-- Countdown 0 10
--
-- >>> consumeClockSec 75 (Countdown 60 10)
-- Timeout
--
-- >>> consumeClockSec 5 (Fischer 60 10)
-- Fischer 65 10
--
-- >>> consumeClockSec 15 (Fischer 60 10)
-- Fischer 55 10
--
-- >>> consumeClockSec 65 (Fischer 60 10)
-- Timeout
--
-- >>> consumeClockSec 10 Timeout
-- Timeout
consumeClockSec :: Sec -> Clock -> Clock
consumeClockSec _ Infinity = Infinity
consumeClockSec sec (Guillotine allot) | a > 0     = Guillotine a
                                       | otherwise = Timeout
  where a = allot - sec
consumeClockSec sec (Countdown allot countdown)
  | a > 0 || c > 0 = Countdown a countdown
  | otherwise      = Timeout
 where
  a = if s >= 0 then s else 0
  c = if s <= 0 then countdown + s else 0
  s = allot - sec
consumeClockSec sec (Fischer allot inc) | a > 0     = Fischer (a + inc) inc
                                        | otherwise = Timeout
  where a = allot - sec
consumeClockSec _ Timeout = Timeout

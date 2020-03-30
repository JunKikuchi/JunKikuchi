module ShogiX.Shogi where

import           ShogiX.Shogi.Types
import           ShogiX.Clocks.Types            ( Sec )

-- | 平手作成
hirate :: Shogi
hirate = undefined

-- | 駒の移動
move :: Sec -> Move -> Shogi -> Shogi
move = undefined

-- | 駒の移動範囲を取得
movables :: Shogi -> Movables
movables = undefined

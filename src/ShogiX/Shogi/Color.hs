module ShogiX.Shogi.Color where

import           ShogiX.Shogi.Types

-- | 先手後手切り替え
turnColor :: Color -> Color
turnColor Black = White
turnColor White = Black

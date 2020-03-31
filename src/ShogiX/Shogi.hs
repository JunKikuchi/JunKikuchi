module ShogiX.Shogi
  ( hirate
  , move
  , movables
  , module ShogiX.Shogi.Types
  , module ShogiX.Clocks
  )
where

import           ShogiX.Shogi.Types
import           ShogiX.Clocks                  ( Sec )

-- | 平手作成
hirate :: Shogi
hirate = undefined

-- | 駒の移動
move :: Sec -> Move -> Shogi -> Shogi
move = undefined

-- | 駒の移動範囲を取得
--
-- rio 導入にあたって一旦 doctest を無効にした
-- >> import qualified Data.Map.Strict as Map
-- >> import qualified Data.List.NonEmpty as NonEmpty
-- >> import qualified ShogiX.Clocks as Clocks
-- >>
-- >> let board = Board (Map.fromList [((F5, R5), Piece Black Pawn)])
-- >> let stands = Stands [] []
-- >> let position = Position Black board stands Clocks.infinity
-- >> let positions = Positions (NonEmpty.fromList [position])
-- >>
-- >> let shogi = Shogi Open positions
-- >> movables shogi
-- fromList [((F5,R5), fromList [((F5, R4),No)]]
movables :: Shogi -> Movables
movables = undefined

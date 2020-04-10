module ShogiX.Shogi.Position
  ( movables
  , droppables
  )
where

import           RIO
import qualified RIO.Map                       as Map
import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Board            as Board
import qualified ShogiX.Shogi.Stand            as Stand

-- | 駒の移動範囲を取得
movables :: Position -> Movables
movables pos = removeCheckedMovables turn board ms
 where
  turn  = positionTurn pos
  board = positionBoard pos
  ms    = Board.movables turn board

-- | 駒の可動範囲から王手になるものを除く
-- >>> removeCheckedMovables Black (Board (Map.fromList [((F5, R9), Piece Black King), ((F5, R8), Piece White Gold)])) (Movables (Map.fromList [((F5, R9), Movable (Map.fromList [((F5, R8), No), ((F4, R9), No)]))]))
-- Movables {unMovables = fromList [((F5,R9),Movable {unMovable = fromList [((F5,R8),No)]})]}
removeCheckedMovables :: Color -> Board -> Movables -> Movables
removeCheckedMovables turn board =
  Movables
    . Map.filter (not . Map.null . unMovable)
    . Map.mapWithKey (removeCheckedMovable turn board)
    . unMovables

-- | 駒の可動範囲から王手になるものを除く
-- >>> removeCheckedMovable Black (Board (Map.fromList [((F5, R9), Piece Black King), ((F5, R8), Piece White Gold)])) (F5, R9) (Movable (Map.fromList [((F5, R8), No), ((F4, R9), No)]))
-- Movable {unMovable = fromList [((F5,R8),No)]}
removeCheckedMovable :: Color -> Board -> SrcSquare -> Movable -> Movable
removeCheckedMovable turn board src =
  Movable . Map.filterWithKey isNotChecked . unMovable
 where
  isNotChecked dest _ = isJust $ do
    mv <- Board.move src False dest board
    guard $ not . Board.checked turn . fst $ mv

-- | 持ち駒の打ち先範囲を取得
droppables :: Position -> Droppables
droppables pos = Stand.droppables turn board stands
 where
  turn   = positionTurn pos
  board  = positionBoard pos
  stands = positionStands pos

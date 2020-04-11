module ShogiX.Shogi.Position
  ( movables
  , droppables
  )
where

import           RIO
import qualified RIO.Map                       as Map
import qualified RIO.Set                       as Set
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
droppables pos = removeCheckedDroppables turn board ds
 where
  turn   = positionTurn pos
  board  = positionBoard pos
  stands = positionStands pos
  ds     = Stand.droppables turn board stands

-- | 持ち駒の打ち先範囲から王手になるものを除く
-- >>> let board = Board (Map.fromList [((F5, R9), Piece Black King), ((F5, R5), Piece White Lance)])
-- >>> let ds = Droppables (Map.fromList [(Pawn, Droppable (Set.fromList [(F5, R8), (F5, R4)]))])
-- >>> removeCheckedDroppables Black board ds
-- Droppables {unDroppables = fromList [(Pawn,Droppable {unDroppable = fromList [(F5,R8)]})]}
removeCheckedDroppables :: Color -> Board -> Droppables -> Droppables
removeCheckedDroppables turn board ds =
  Droppables $ Droppable . Set.intersection ss . unDroppable <$> d
 where
  ss = Set.filter (removeCheckedDroppable turn board)
    $ Map.foldr (Set.union . unDroppable) Set.empty d
  d = unDroppables ds

-- | 持ち駒の打ち先範囲から王手になるものを除く
-- >>> let board = Board (Map.fromList [((F5, R9), Piece Black King), ((F5, R5), Piece White Lance)])
-- >>> removeCheckedDroppable Black board (F5, R8)
-- True
-- >>> removeCheckedDroppable Black board (F5, R4)
-- False
removeCheckedDroppable :: Color -> Board -> DestSquare -> Bool
removeCheckedDroppable turn board dest = isJust $ do
  bd <- Board.drop turn Pawn dest board
  guard $ not $ Board.checked turn bd

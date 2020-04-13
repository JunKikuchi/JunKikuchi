module ShogiX.Shogi.Position
  ( move
  , ShogiX.Shogi.Position.drop
  , movables
  , droppables
  )
where

import           RIO
import qualified RIO.Map                       as Map
import qualified RIO.Set                       as Set
import           ShogiX.Clocks.Types
import qualified ShogiX.Clocks                 as Clocks
import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Color            as Color
import qualified ShogiX.Shogi.Board            as Board
import qualified ShogiX.Shogi.Stands           as Stands

-- | 駒の移動
move
  :: SrcSquare -> Promotion -> DestSquare -> Sec -> Position -> Maybe Position
move src promo dest sec pos = do
  (newBoard, captured) <- Board.move src promo dest (positionBoard pos)
  pure $ pos { positionTurn   = Color.turnColor turn
             , positionBoard  = newBoard
             , positionStands = Stands.add turn captured stands
             , positionClocks = Clocks.consume sec turn clocks
             }
 where
  turn   = positionTurn pos
  stands = positionStands pos
  clocks = positionClocks pos

-- | 駒の打ち込み
drop :: PieceType -> DestSquare -> Sec -> Position -> Maybe Position
drop = undefined

-- | 駒の移動範囲を取得
movables :: Position -> Movables
movables pos = removeCheckedMovables turn board ms
 where
  turn  = positionTurn pos
  board = positionBoard pos
  ms    = Board.movables turn board

-- | 駒の可動範囲から王手になるものを除く
-- >>> import qualified ShogiX.Shogi.Movables as Movables
-- >>> let board = Board.fromList [((F5, R9), Piece Black King), ((F5, R8), Piece White Gold)]
-- >>> let ms = Movables.fromList [((F5, R9), [((F5, R8), No), ((F4, R9), No)])]
-- >>> removeCheckedMovables Black board ms
-- Movables {unMovables = fromList [((F5,R9),Movable {unMovable = fromList [((F5,R8),No)]})]}
removeCheckedMovables :: Color -> Board -> Movables -> Movables
removeCheckedMovables turn board =
  Movables
    . Map.filter (not . Map.null . unMovable)
    . Map.mapWithKey (removeCheckedMovable turn board)
    . unMovables

-- | 駒の可動範囲から王手になるものを除く
-- >>> import qualified ShogiX.Shogi.Movable as Movable
-- >>> let board = Board.fromList [((F5, R9), Piece Black King), ((F5, R8), Piece White Gold)]
-- >>> let m = Movable.fromList [((F5, R8), No), ((F4, R9), No)]
-- >>> removeCheckedMovable Black board (F5, R9) m
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
  ds     = Stands.droppables turn board stands

-- | 持ち駒の打ち先範囲から王手になるものを除く
-- >>> import qualified ShogiX.Shogi.Droppables as Droppables
-- >>> let board = Board.fromList [((F5, R9), Piece Black King), ((F5, R5), Piece White Lance)]
-- >>> let ds = Droppables.fromList [(Pawn, [(F5, R8), (F5, R4)])]
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
-- >>> let board = Board.fromList [((F5, R9), Piece Black King), ((F5, R5), Piece White Lance)]
-- >>> removeCheckedDroppable Black board (F5, R8)
-- True
-- >>> removeCheckedDroppable Black board (F5, R4)
-- False
removeCheckedDroppable :: Color -> Board -> DestSquare -> Bool
removeCheckedDroppable turn board dest = isJust $ do
  bd <- Board.drop turn Pawn dest board
  guard $ not $ Board.checked turn bd

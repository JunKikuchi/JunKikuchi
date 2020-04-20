module ShogiX.Shogi.Position
  ( move
  , ShogiX.Shogi.Position.drop
  , checked
  , mate
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
import qualified ShogiX.Shogi.Movables         as Movables
import qualified ShogiX.Shogi.Droppables       as Droppables

-- | 駒の移動
{-# ANN module "HLint: ignore Reduce duplication" #-}
move
  :: SrcSquare
  -> Promotion
  -> DestSquare
  -> Sec
  -> Position
  -> Either CloseStatus Position
move src promo dest sec pos = do
  -- 持ち時間チェック
  let newClocks = Clocks.consume sec turn clocks
      clock     = Clocks.getClock turn newClocks
  when (clock == Clocks.Timeout) (Left ShogiX.Shogi.Types.Timeout)
  -- 駒移動
  (newBoard, captured) <- illegalCheck (Board.move src promo dest board)
  let newPos = pos { positionTurn   = Color.turnColor turn
                   , positionBoard  = newBoard
                   , positionStands = Stands.add turn captured stands
                   , positionClocks = newClocks
                   }
  -- 王手回避チェック
  when (checked turn newPos) (Left (Illegal AbandonCheck))
  pure newPos
 where
  illegalCheck = maybe (Left (Illegal IllegalMove)) pure
  turn         = positionTurn pos
  board        = positionBoard pos
  stands       = positionStands pos
  clocks       = positionClocks pos

-- | 駒の打ち込み
drop
  :: PieceType -> DestSquare -> Sec -> Position -> Either CloseStatus Position
drop pt dest sec pos = do
  -- 持ち時間チェック
  let newClocks = Clocks.consume sec turn clocks
      clock     = Clocks.getClock turn newClocks
  when (clock == Clocks.Timeout) (Left ShogiX.Shogi.Types.Timeout)
  -- 駒の打ち込み
  newStand <- illegalCheck (Stands.drop turn pt stands)
  newBoard <- illegalCheck (Board.drop turn pt dest board)
  let newPos = pos { positionTurn   = Color.turnColor turn
                   , positionBoard  = newBoard
                   , positionStands = newStand
                   , positionClocks = Clocks.consume sec turn clocks
                   }
  -- 王手回避チェック
  when (checked turn newPos)       (Left (Illegal AbandonCheck))
  -- 打ち歩詰めチェック
  when (mate newPos && pt == Pawn) (Left (Illegal DroppedPawnMate))
  pure newPos
 where
  illegalCheck = maybe (Left (Illegal IllegalDrop)) pure
  turn         = positionTurn pos
  board        = positionBoard pos
  stands       = positionStands pos
  clocks       = positionClocks pos

-- | 王手判定
checked :: Color -> Position -> Bool
checked color = Board.checked color . positionBoard

-- | 詰み判定
mate :: Position -> Bool
mate pos = hasKing && nullMovables && nullDroppables
 where
  turn           = positionTurn pos
  hasKing        = not $ Set.null $ Board.kingSquares turn (positionBoard pos)
  nullMovables   = Movables.null $ movables pos
  nullDroppables = Droppables.null $ droppables pos

-- | 駒の移動範囲を取得
movables :: Position -> Movables
movables pos = colorMovables (positionTurn pos) pos

-- | 駒の移動範囲を取得
colorMovables :: Color -> Position -> Movables
colorMovables color pos = removeCheckedMovables color board ms
 where
  board = positionBoard pos
  ms    = Board.movables color board

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
    guard (not . Board.checked turn . fst $ mv)

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

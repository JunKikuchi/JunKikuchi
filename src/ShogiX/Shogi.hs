module ShogiX.Shogi
  ( hirate
  , move
  , movables
  , droppables
  , shogiPosition
  , module ShogiX.Shogi.Types
  , module ShogiX.Clocks
  )
where

import           RIO
import qualified RIO.Map                       as Map
import qualified RIO.NonEmpty                  as NE
import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Position         as Position
import           ShogiX.Clocks                  ( Sec )

-- | 平手作成
hirate :: Shogi
hirate = undefined

-- | 駒の移動
move :: Move -> Sec -> Shogi -> Shogi
move = undefined

-- | 駒の移動範囲を取得
--
-- >>> import RIO
-- >>> import qualified ShogiX.Shogi.Board as Board
-- >>> import qualified ShogiX.Shogi.Stands as Stands
-- >>> import qualified ShogiX.Clocks as Clocks
-- >>>
-- >>> let board = Board.fromList [((F5, R5), Piece Black Pawn)]
-- >>> let position = Position Black board Stands.empty Clocks.infinity
-- >>> let shogi = Shogi Open (Positions (position :| []))
-- >>>
-- >>> movables shogi
-- Movables {unMovables = fromList [((F5,R5),Movable {unMovable = fromList [((F5,R4),No)]})]}
movables :: Shogi -> Movables
movables shogi | status == Open = Position.movables pos
               | otherwise      = empty
 where
  status = shogiStatus shogi
  empty  = Movables Map.empty
  pos    = shogiPosition shogi

-- | 持ち駒の打ち先範囲を取得
--
-- >>> import RIO
-- >>> import qualified RIO.Map as Map
-- >>> import qualified ShogiX.Shogi.Board as Board
-- >>> import qualified ShogiX.Shogi.Stands as Stands
-- >>> import qualified ShogiX.Clocks as Clocks
-- >>>
-- >>> let board = Board.fromList [((file, R3), Piece Black Pawn) | file <- [F9 .. F2]]
-- >>> let stands = Stands.fromList [(Pawn, 1)] []
-- >>> let position = Position Black board stands Clocks.infinity
-- >>> let shogi = Shogi Open (Positions (position :| []))
-- >>>
-- >>> droppables shogi
-- Droppables {unDroppables = fromList [(Pawn,Droppable {unDroppable = fromList [(F1,R2),(F1,R3),(F1,R4),(F1,R5),(F1,R6),(F1,R7),(F1,R8),(F1,R9)]})]}
droppables :: Shogi -> Droppables
droppables shogi | status == Open = Position.droppables pos
                 | otherwise      = empty
 where
  status = shogiStatus shogi
  empty  = Droppables Map.empty
  pos    = shogiPosition shogi

-- | 最新の局面取得
shogiPosition :: Shogi -> Position
shogiPosition = NE.head . unPositions . shogiPositions

module ShogiX.Shogi
  ( hirate
  , update
  , movables
  , droppables
  , shogiPosition
  , module ShogiX.Shogi.Types
  )
where

import           RIO
import qualified RIO.NonEmpty                  as NE
import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Color            as Color
import qualified ShogiX.Shogi.Position         as Position
import qualified ShogiX.Shogi.Movables         as Movables
import qualified ShogiX.Shogi.Droppables       as Droppables
import           ShogiX.Clocks                  ( Sec )
import qualified ShogiX.Clocks                 as Clocks

-- | 平手作成
hirate :: Shogi
hirate = undefined

-- | 更新
--
-- >>> import RIO
-- >>> import qualified ShogiX.Shogi.Board as Board
-- >>> import qualified ShogiX.Shogi.Stands as Stands
-- >>> import qualified ShogiX.Clocks as Clocks
-- >>>
-- >>> let board = Board.fromList [((F5, R9), Piece Black King), ((F5, R1), Piece White King)]
-- >>> let stands = Stands.fromList [(Pawn, 1)] [(Pawn, 1)]
-- >>> let position = Position Black board stands Clocks.infinity
-- >>> let shogi = Shogi Open (Positions (position :| []))
-- >>>
-- >>> update (Move (F5, R9) False (F5, R8)) 3 shogi
-- Shogi {shogiStatus = Open, shogiPositions = Positions {unPositions = Position {positionTurn = White, positionBoard = Board {unBoard = fromList [((F5,R1),Piece {pieceColor = White, pieceType = King}),((F5,R8),Piece {pieceColor = Black, pieceType = King})]}, positionStands = Stands {blackStand = Stand {unStand = fromList [(Pawn,1)]}, whiteStand = Stand {unStand = fromList [(Pawn,1)]}}, positionClocks = Clocks {blackClock = Infinity, whiteClock = Infinity}} :| [Position {positionTurn = Black, positionBoard = Board {unBoard = fromList [((F5,R1),Piece {pieceColor = White, pieceType = King}),((F5,R9),Piece {pieceColor = Black, pieceType = King})]}, positionStands = Stands {blackStand = Stand {unStand = fromList [(Pawn,1)]}, whiteStand = Stand {unStand = fromList [(Pawn,1)]}}, positionClocks = Clocks {blackClock = Infinity, whiteClock = Infinity}}]}}
-- >>>
-- >>> update (Drop Pawn (F5, R8)) 3 shogi
-- Shogi {shogiStatus = Open, shogiPositions = Positions {unPositions = Position {positionTurn = White, positionBoard = Board {unBoard = fromList [((F5,R1),Piece {pieceColor = White, pieceType = King}),((F5,R8),Piece {pieceColor = Black, pieceType = Pawn}),((F5,R9),Piece {pieceColor = Black, pieceType = King})]}, positionStands = Stands {blackStand = Stand {unStand = fromList []}, whiteStand = Stand {unStand = fromList [(Pawn,1)]}}, positionClocks = Clocks {blackClock = Infinity, whiteClock = Infinity}} :| [Position {positionTurn = Black, positionBoard = Board {unBoard = fromList [((F5,R1),Piece {pieceColor = White, pieceType = King}),((F5,R9),Piece {pieceColor = Black, pieceType = King})]}, positionStands = Stands {blackStand = Stand {unStand = fromList [(Pawn,1)]}, whiteStand = Stand {unStand = fromList [(Pawn,1)]}}, positionClocks = Clocks {blackClock = Infinity, whiteClock = Infinity}}]}}
update :: Update -> Sec -> Shogi -> Shogi
update (Move s p d) = updateShogi (Position.move s p d)
update (Drop pt d ) = updateShogi (Position.drop pt d)
update CloseResign  = closeResign
update CloseImpasse = closeImpasse
update ConsumeTime  = consumeTime

-- | 将棋の駒移動
updateShogi
  :: (Position -> Either CloseStatus Position) -> Sec -> Shogi -> Shogi
updateShogi up sec shogi = unEither $ do
  pos <- shogiConsumeTime sec shogi
  pure $ put shogi $ either (close pos) continue $ up pos
 where
  continue pos | Position.mate pos = close pos Mate
               | otherwise         = (shogiStatus shogi, pos)
  close pos status = (Closed (winner pos) status, pos)

-- | 投了
closeResign :: Sec -> Shogi -> Shogi
closeResign sec shogi = unEither $ do
  pos <- shogiConsumeTime sec shogi
  pure $ put shogi (Closed (winner pos) Resign, pos)

-- | 持将棋
closeImpasse :: Sec -> Shogi -> Shogi
closeImpasse sec shogi = unEither $ do
  pos <- shogiConsumeTime sec shogi
  pure $ put shogi (Draw Impasse, pos)

-- | 将棋データ更新
put :: Shogi -> (Status, Position) -> Shogi
put shogi (status, pos) = consPosition pos shogi { shogiStatus = status }

-- | 手番では無い側
winner :: Position -> Color
winner = Color.turnColor . positionTurn

-- | 対局時計の経過時間チェック
consumeTime :: Sec -> Shogi -> Shogi
consumeTime sec shogi = unEither $ do
  _ <- shogiConsumeTime sec shogi
  pure shogi

-- | Either を外す
unEither :: Either Shogi Shogi -> Shogi
unEither = either id id

-- | 対局時計の時間を進める
shogiConsumeTime :: Sec -> Shogi -> Either Shogi Position
shogiConsumeTime sec shogi | clock == Clocks.Timeout = Left closed
                           | otherwise               = Right newPos
 where
  clock = Clocks.getClock turn $ positionClocks newPos
  closed =
    consPosition newPos shogi { shogiStatus = Closed (winner pos) Timeout }
  turn   = positionTurn pos
  newPos = Position.consumeTime sec pos
  pos    = shogiPosition shogi

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
               | otherwise      = Movables.empty
 where
  status = shogiStatus shogi
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
                 | otherwise      = Droppables.empty
 where
  status = shogiStatus shogi
  pos    = shogiPosition shogi

-- | 最新の局面取得
shogiPosition :: Shogi -> Position
shogiPosition = NE.head . unPositions . shogiPositions

-- | 局面を追加
consPosition :: Position -> Shogi -> Shogi
consPosition pos shogi = shogi
  { shogiPositions = Positions
                     . (pos NE.<|)
                     . unPositions
                     . shogiPositions
                     $ shogi
  }

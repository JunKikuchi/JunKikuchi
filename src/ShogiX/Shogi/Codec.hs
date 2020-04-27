module ShogiX.Shogi.Codec
  ( load
  , dump
  )
where

import           RIO
import qualified RIO.NonEmpty                  as NE
import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Board            as Board
import qualified ShogiX.Shogi.Stands           as Stands
import qualified ShogiX.Shogi.Updates          as Updates
import qualified ShogiX.Shogi.Codec.Types      as C

-- | 将棋データロード
--
-- >>> import RIO.NonEmpty ((<|))
-- >>> import qualified RIO.NonEmpty as NE
-- >>> import qualified ShogiX.Clocks as Clocks
-- >>>
-- >>> let positions = (White, [((F5, R8), Piece Black King), ((F5, R1), Piece White King)], ([], []), Clocks.infinity) <| (Black, [((F5, R9), Piece Black King), ((F5, R1), Piece White King)], ([], []), Clocks.infinity) :| []
-- >>> let updates = [(Move (F5, R9) False (F5, R8), 3)]
-- >>> load (Open, positions, updates)
-- Shogi {shogiStatus = Open, shogiPositions = Positions {unPositions = Position {positionTurn = White, positionBoard = Board {unBoard = fromList [((F5,R1),Piece {pieceColor = White, pieceType = King}),((F5,R8),Piece {pieceColor = Black, pieceType = King})]}, positionStands = Stands {blackStand = Stand {unStand = fromList []}, whiteStand = Stand {unStand = fromList []}}, positionClocks = Clocks {blackClock = Infinity, whiteClock = Infinity}} :| [Position {positionTurn = Black, positionBoard = Board {unBoard = fromList [((F5,R1),Piece {pieceColor = White, pieceType = King}),((F5,R9),Piece {pieceColor = Black, pieceType = King})]}, positionStands = Stands {blackStand = Stand {unStand = fromList []}, whiteStand = Stand {unStand = fromList []}}, positionClocks = Clocks {blackClock = Infinity, whiteClock = Infinity}}]}, shogiUpdates = Updates {unUpdates = [(Move (F5,R9) False (F5,R8),3)]}}
load :: C.Shogi -> Shogi
load (status, positions, updates) =
  Shogi status (loadPositions positions) (loadUpdates updates)

loadPositions :: C.Positions -> Positions
loadPositions = Positions . NE.map loadPosition

loadPosition :: C.Position -> Position
loadPosition (t, b, ss, cs) =
  Position t (Board.fromList b) (Stands.fromTuple ss) cs

loadUpdates :: C.Updates -> Updates
loadUpdates = foldr Updates.cons Updates.empty

-- | 将棋データダンプ
--
-- >>> import RIO.NonEmpty ((<|))
-- >>> import qualified ShogiX.Clocks as Clocks
-- >>> import qualified ShogiX.Shogi.Positions as Positions
-- >>>
-- >>> let position1 = Position White (Board.fromList [((F5, R1), Piece White King), ((F5, R8), Piece Black King)]) (Stands.fromTuple ([], [])) Clocks.infinity
-- >>> let position2 = Position Black (Board.fromList [((F5, R1), Piece White King), ((F5, R9), Piece Black King)]) (Stands.fromTuple ([], [])) Clocks.infinity
-- >>> let positions = Positions.cons position1 (Positions.singleton position2)
-- >>> let updates = Updates [(Move (F5,R9) False (F5,R8),3)]
-- >>> let shogi = Shogi Open positions updates
-- >>> dump shogi
-- (Open,(White,[((F5,R1),Piece {pieceColor = White, pieceType = King}),((F5,R8),Piece {pieceColor = Black, pieceType = King})],([],[]),Clocks {blackClock = Infinity, whiteClock = Infinity}) :| [(Black,[((F5,R1),Piece {pieceColor = White, pieceType = King}),((F5,R9),Piece {pieceColor = Black, pieceType = King})],([],[]),Clocks {blackClock = Infinity, whiteClock = Infinity})],[(Move (F5,R9) False (F5,R8),3)])
dump :: Shogi -> C.Shogi
dump shogi = (status, positions, updates)
 where
  status    = shogiStatus shogi
  positions = dumpPositions $ shogiPositions shogi
  updates   = dumpUpdates $ shogiUpdates shogi

dumpPositions :: Positions -> C.Positions
dumpPositions = NE.map dumpPosition . unPositions

dumpPosition :: Position -> C.Position
dumpPosition pos = (turn, board, stands, clocks)
 where
  turn   = positionTurn pos
  board  = Board.toList $ positionBoard pos
  stands = Stands.toTuple $ positionStands pos
  clocks = positionClocks pos

dumpUpdates :: Updates -> C.Updates
dumpUpdates = unUpdates

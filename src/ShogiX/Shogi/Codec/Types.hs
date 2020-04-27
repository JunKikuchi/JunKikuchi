module ShogiX.Shogi.Codec.Types where

import           RIO
import qualified RIO.NonEmpty                  as NE
import qualified ShogiX.Shogi.Types            as S
import           ShogiX.Clocks.Types

type Shogi      = (S.Status, Positions, Updates)
type Positions  = NE.NonEmpty Position
type Position   = (S.Turn, Board, Stands, Clocks)
type Board      = [(S.Square, S.Piece)]
type Stands     = (BlackStand, WhiteStand)
type BlackStand = Stand
type WhiteStand = Stand
type Stand      = [(S.PieceType, Int)]
type Updates    = [Update]
type Update     = (S.Update, Sec)

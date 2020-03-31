module ShogiX.Shogi.Board where

import           RIO
import qualified RIO.Map                       as Map
import           ShogiX.Shogi.Types

-- | 駒の移動範囲を取得
movables :: Color -> Board -> Movables
movables color board = Movables
  $ Map.foldrWithKey build Map.empty (unBoard board)
 where
  build square piece acc
    | pieceColor piece == color && not (Map.null m) = Map.insert square m acc
    | otherwise = acc
    where m = movable (pieceType piece) square board

-- | 駒の移動先を取得
movable :: PieceType -> SrcSquare -> Board -> Movable
movable Pawn           = undefined
movable Lance          = undefined
movable Knight         = undefined
movable Silver         = undefined
movable Gold           = undefined
movable Bishop         = undefined
movable Rook           = undefined
movable King           = undefined
movable PromotedPawn   = undefined
movable PromotedLance  = undefined
movable PromotedKnight = undefined
movable PromotedSilver = undefined
movable PromotedBishop = undefined
movable PromotedRook   = undefined

{-# LANGUAGE RankNTypes #-}
module ShogiX.Shogi.Piece where

import           RIO
import qualified RIO.Map                       as Map
import           ShogiX.Shogi.Types

-- | 駒の移動先を取得
movable :: Piece -> SrcSquare -> Map Square Color -> Movable
movable piece s sc = case pieceType piece of
  Pawn           -> buildPromo $ pawn s
  Lance          -> undefined
  Knight         -> undefined
  Silver         -> undefined
  Gold           -> undefined
  Bishop         -> undefined
  Rook           -> undefined
  King           -> undefined
  PromotedPawn   -> undefined
  PromotedLance  -> undefined
  PromotedKnight -> undefined
  PromotedSilver -> undefined
  PromotedBishop -> undefined
  PromotedRook   -> undefined
 where
  buildPromo = build promo color sc
  color      = pieceColor piece

type Movements = [[DestSquare]]
type Promo = Square -> Promotable

build :: Promo -> Color -> Map Square Color -> Movements -> Movable
build p c sc mss = Map.fromList [ (m, p m) | ms <- mss, m <- hit c sc ms ]

-- | 駒があるマス目まで可動範囲を刈り取る
--
-- >>> hit Black Map.empty [(F5, R3), (F5, R2), (F5, R1)]
-- [(F5,R3),(F5,R2),(F5,R1)]
--
-- >>> hit Black (Map.fromList [((F5, R2), Black)]) [(F5, R3), (F5, R2), (F5, R1)]
-- [(F5,R3)]
--
-- >>> hit Black (Map.fromList [((F5, R2), White)]) [(F5, R3), (F5, R2), (F5, R1)]
-- [(F5,R3),(F5,R2)]
--
-- >>> hit Black Map.empty []
-- []
hit :: Color -> Map Square Color -> [Square] -> [Square]
hit _ _  []       = []
hit c sc (s : ss) = case Map.lookup s sc of
  Just color -> if color == c then [] else [s]
  Nothing    -> s : hit c sc ss

-- | 成り不成
promo :: Promo
promo (_, R1) = Must
promo (_, R2) = Option
promo (_, R3) = Option
promo _       = No

-- | 歩兵の可動範囲
--
-- >>> pawn (F5, R5)
-- [[(F5,R4)]]
pawn :: SrcSquare -> Movements
pawn s = [one $ up s]

-- | マス目リストから最初のひとつ取得
--
-- >>> one [(F1, R1), (F2, R2), (F3, R3)]
-- [(F1,R1)]
--
-- >>> one []
-- []
one :: [Square] -> [Square]
one = take 1

-- | 上へのマス目リスト生成
--
-- >>> up (F1, R5)
-- [(F1,R4),(F1,R3),(F1,R2),(F1,R1)]
--
-- >>> up (F1, R1)
-- []
up :: Square -> [Square]
up (file, rank) = [ (file, r) | r <- drop 1 $ reverse [minBound .. rank] ]

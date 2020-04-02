{-# LANGUAGE RankNTypes #-}
module ShogiX.Shogi.Piece where

import           RIO
import           RIO.Partial                    ( succ
                                                , pred
                                                )
import qualified RIO.Map                       as Map
import           ShogiX.Shogi.Types

-- | 駒の移動先を取得
--
-- >>> import RIO.Map as Map
-- >>> movable (Piece Black Pawn) (F5, R5) (Map.fromList [((F5, R5), Black)])
-- Movable {unMovable = fromList [((F5,R4),No)]}
--
-- >>> movable (Piece White Pawn) (F5, R5) (Map.fromList [((F5, R5), Black)])
-- Movable {unMovable = fromList [((F5,R6),No)]}
movable :: Piece -> SrcSquare -> Map Square Color -> Movable
movable piece s sc = case pieceType piece of
  Pawn           -> promoMovables $ pawn color s
  Lance          -> promoMovables $ lance color s
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
  promoMovables = movables promo color sc
  color         = pieceColor piece

-- | 駒の可動範囲リスト
type Movements = [Movement]

-- | 駒の可動範囲
type Movement  = [DestSquare]

-- | 成り不成判定
type Promo = Square -> Promotable

-- | 駒の可動範囲を生成
movables :: Promo -> Color -> Map Square Color -> Movements -> Movable
movables p c sc mss =
  Movable (Map.fromList [ (m, p m) | ms <- mss, m <- takeMovables c sc ms ])

-- | 駒があるマス目まで可動範囲を刈り取る
--
-- >>> takeMovables Black Map.empty [(F5, R3), (F5, R2), (F5, R1)]
-- [(F5,R3),(F5,R2),(F5,R1)]
--
-- >>> takeMovables Black (Map.fromList [((F5, R2), Black)]) [(F5, R3), (F5, R2), (F5, R1)]
-- [(F5,R3)]
--
-- >>> takeMovables Black (Map.fromList [((F5, R2), White)]) [(F5, R3), (F5, R2), (F5, R1)]
-- [(F5,R3),(F5,R2)]
--
-- >>> takeMovables Black Map.empty []
-- []
takeMovables :: Color -> Map Square Color -> [Square] -> [Square]
takeMovables _ _  []       = []
takeMovables c sc (s : ss) = case Map.lookup s sc of
  Just color -> if color == c then [] else [s]
  Nothing    -> s : takeMovables c sc ss

-- | 成り不成判定
promo :: Promo
promo (_, R1) = Must
promo (_, R2) = Option
promo (_, R3) = Option
promo _       = No

-- | 歩兵の可動範囲
--
-- >>> pawn Black (F5, R5)
-- [[(F5,R4)]]
--
-- >>> pawn White (F5, R5)
-- [[(F5,R6)]]
pawn :: Color -> SrcSquare -> Movements
pawn c s = [one $ forward c s]

-- | 香車の可動範囲
lance :: Color -> SrcSquare -> Movements
lance c s = [forward c s]

-- | 桂馬の可動範囲
knight :: Color -> SrcSquare -> Movements
knight c (file, rank) = [lf c, rf c]
 where
  lf Black | rank < R3 || file < F8 = []
           | otherwise              = [(l, u)]
  lf White | rank > R7 || file > F2 = []
           | otherwise              = [(r, d)]
  rf Black | rank < R3 || file > F2 = []
           | otherwise              = [(r, u)]
  rf White | rank > R7 || file < F8 = []
           | otherwise              = [(l, d)]
  l = pred file
  r = succ file
  u = pred . pred $ rank
  d = succ . succ $ rank

-- | マス目リストから最初のひとつ取得
--
-- >>> one [(F1, R1), (F2, R2), (F3, R3)]
-- [(F1,R1)]
--
-- >>> one []
-- []
one :: Movement -> Movement
one = take 1

-- | 前方マス目リスト生成
--
-- >>> forward Black (F1, R5)
-- [(F1,R4),(F1,R3),(F1,R2),(F1,R1)]
--
-- >>> forward White (F1, R5)
-- [(F1,R6),(F1,R7),(F1,R8),(F1,R9)]
forward :: Color -> SquareMovement
forward Black = up
forward White = down

-- | マス目リスト生成
type SquareMovement = Square -> Movement

-- | 左上へのマス目リスト生成
--
-- >>> leftUp (F5, R5)
-- [(F6,R4),(F7,R3),(F8,R2),(F9,R1)]
--
-- >>> leftUp (F9, R1)
-- []
leftUp :: SquareMovement
leftUp (file, rank) = zip (preds file) (preds rank)

-- | 上へのマス目リスト生成
--
-- >>> up (F1, R5)
-- [(F1,R4),(F1,R3),(F1,R2),(F1,R1)]
--
-- >>> up (F1, R1)
-- []
up :: SquareMovement
up (file, rank) = [ (file, r) | r <- preds rank ]

-- | 右上へのマス目リスト生成
--
-- >>> rightUp (F5, R5)
-- [(F4,R4),(F3,R3),(F2,R2),(F1,R1)]
--
-- >>> rightUp (F1, R1)
-- []
rightUp :: SquareMovement
rightUp (file, rank) = zip (succs file) (preds rank)

-- | 右へのマス目リスト生成
--
-- >>> right (F5, R5)
-- [(F4,R5),(F3,R5),(F2,R5),(F1,R5)]
--
-- >>> right (F1, R5)
-- []
right :: SquareMovement
right (file, rank) = [ (f, rank) | f <- succs file ]

-- | 右下へのマス目リスト生成
--
-- >>> rightDown (F5, R5)
-- [(F4,R6),(F3,R7),(F2,R8),(F1,R9)]
--
-- >>> rightDown (F1, R9)
-- []
rightDown :: SquareMovement
rightDown (file, rank) = zip (succs file) (succs rank)

-- | 下へのマス目リスト生成
--
-- >>> down (F1, R5)
-- [(F1,R6),(F1,R7),(F1,R8),(F1,R9)]
--
-- >>> down (F1, R9)
-- []
down :: SquareMovement
down (file, rank) = [ (file, r) | r <- succs rank ]

-- | 左下へのマス目リスト生成
--
-- >>> leftDown (F5, R5)
-- [(F6,R6),(F7,R7),(F8,R8),(F9,R9)]
--
-- >>> leftDown (F9, R9)
-- []
leftDown :: SquareMovement
leftDown (file, rank) = zip (preds file) (succs rank)

-- | 左へのマス目リスト生成
--
-- >>> left (F5, R5)
-- [(F6,R5),(F7,R5),(F8,R5),(F9,R5)]
--
-- >>> left (F9, R5)
-- []
left :: SquareMovement
left (file, rank) = [ (f, rank) | f <- preds file ]

-- | 減少リスト生成
--
-- >>> preds R5
-- [R4,R3,R2,R1]
preds :: forall a . (Enum a, Bounded a) => a -> [a]
preds a = drop 1 $ reverse [minBound .. a]

-- | 増加リスト生成
--
-- >>> succs R5
-- [R6,R7,R8,R9]
succs :: forall a . (Enum a, Bounded a) => a -> [a]
succs a = drop 1 [a .. maxBound]

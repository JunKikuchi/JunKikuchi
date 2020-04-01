module ShogiX.Shogi.Piece where

import           RIO
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
forward :: Color -> Square -> Movement
forward Black = up
forward White = down

-- | 上へのマス目リスト生成
--
-- >>> up (F1, R5)
-- [(F1,R4),(F1,R3),(F1,R2),(F1,R1)]
--
-- >>> up (F1, R1)
-- []
up :: Square -> Movement
up (file, rank) = [ (file, r) | r <- drop 1 $ reverse [minBound .. rank] ]

-- | 下へのマス目リスト生成
--
-- >>> down (F1, R5)
-- [(F1,R6),(F1,R7),(F1,R8),(F1,R9)]
--
-- >>> down (F1, R9)
-- []
down :: Square -> Movement
down (file, rank) = [ (file, r) | r <- drop 1 [rank .. maxBound] ]

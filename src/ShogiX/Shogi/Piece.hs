{-# LANGUAGE RankNTypes #-}
module ShogiX.Shogi.Piece
  ( movable
  , droppable
  , promote
  , demote
  , pawn
  , lance
  , knight
  , silver
  , gold
  , bishop
  , rook
  , king
  )
where

import           RIO
import           RIO.Partial                    ( succ
                                                , pred
                                                )
import qualified RIO.Map                       as Map
import qualified RIO.Set                       as Set
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
movable (Piece color pt) src sc = Movable mv
 where
  mv = Map.fromList
    [ (m, promoExam color pt m) | ms <- mss, m <- takeMovables color sc ms ]
  mss            = pieceMovements color src
  pieceMovements = case pt of
    Pawn           -> pawn
    Lance          -> lance
    Knight         -> knight
    Silver         -> silver
    Gold           -> gold
    Bishop         -> bishop
    Rook           -> rook
    King           -> king
    PromotedPawn   -> gold
    PromotedLance  -> gold
    PromotedKnight -> gold
    PromotedSilver -> gold
    PromotedBishop -> promotedBishop
    PromotedRook   -> promotedRook

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
promoExam :: Color -> PieceType -> Square -> Promotable
promoExam _     King           _       = No
promoExam _     Gold           _       = No
promoExam _     PromotedRook   _       = No
promoExam _     PromotedBishop _       = No
promoExam _     PromotedSilver _       = No
promoExam _     PromotedLance  _       = No
promoExam _     PromotedKnight _       = No
promoExam _     PromotedPawn   _       = No
promoExam Black Rook           (_, R1) = Option
promoExam Black Bishop         (_, R1) = Option
promoExam Black _              (_, R1) = Must
promoExam Black _              (_, R2) = Option
promoExam Black _              (_, R3) = Option
promoExam Black _              _       = No
promoExam White Rook           (_, R9) = Option
promoExam White Bishop         (_, R9) = Option
promoExam White _              (_, R9) = Must
promoExam White _              (_, R8) = Option
promoExam White _              (_, R7) = Option
promoExam White _              _       = No

-- | 駒の可動範囲
type PieceMovements = Color -> SrcSquare -> Movements

-- | 駒の可動範囲リスト
type Movements = [Movement]

-- | 駒の可動範囲
type Movement  = [DestSquare]

-- | 歩兵の可動範囲
--
-- >>> pawn Black (F5, R5)
-- [[(F5,R4)]]
--
-- >>> pawn White (F5, R5)
-- [[(F5,R6)]]
pawn :: PieceMovements
pawn c s = [one $ forward c s]

-- | 香車の可動範囲
lance :: PieceMovements
lance c s = [forward c s]

-- | 桂馬の可動範囲
knight :: PieceMovements
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

-- | 銀将の可動範囲
silver :: PieceMovements
silver c s =
  (\f -> one $ f c s)
    <$> [leftForward, forward, rightForward, rightBackward, leftBackward]

-- | 金将の可動範囲
gold :: PieceMovements
gold c s =
  (\f -> one $ f c s)
    <$> [leftForward, forward, rightForward, const right, backward, const left]

-- | 角行の可動範囲
bishop :: PieceMovements
bishop _ s = (\f -> f s) <$> [leftUp, rightUp, rightDown, leftDown]

-- | 飛車の可動範囲
rook :: PieceMovements
rook _ s = (\f -> f s) <$> [up, right, down, left]

-- | 玉将の可動範囲
king :: PieceMovements
king c s =
  (\f -> one $ f c s)
    <$> [ leftForward
        , forward
        , rightForward
        , const right
        , rightBackward
        , backward
        , leftBackward
        , const left
        ]

-- | 龍馬の可動範囲
promotedBishop :: PieceMovements
promotedBishop _ s =
  (\f -> f s)
    <$> [ leftUp
        , rightUp
        , rightDown
        , leftDown
        , one . up
        , one . right
        , one . down
        , one . left
        ]

-- | 龍王の可動範囲
promotedRook :: PieceMovements
promotedRook _ s =
  (\f -> f s)
    <$> [ up
        , right
        , down
        , left
        , one . leftUp
        , one . rightUp
        , one . rightDown
        , one . leftDown
        ]

-- | マス目リストから最初のひとつ取得
--
-- >>> one [(F1, R1), (F2, R2), (F3, R3)]
-- [(F1,R1)]
--
-- >>> one []
-- []
one :: Movement -> Movement
one = take 1

-- | 左前方マス目リスト生成
--
-- >>> leftForward Black (F5, R5)
-- [(F6,R4),(F7,R3),(F8,R2),(F9,R1)]
--
-- >>> leftForward White (F5, R5)
-- [(F6,R6),(F7,R7),(F8,R8),(F9,R9)]
leftForward :: Color -> SquareMovement
leftForward Black = leftUp
leftForward White = leftDown

-- | 前方マス目リスト生成
--
-- >>> forward Black (F5, R5)
-- [(F5,R4),(F5,R3),(F5,R2),(F5,R1)]
--
-- >>> forward White (F5, R5)
-- [(F5,R6),(F5,R7),(F5,R8),(F5,R9)]
forward :: Color -> SquareMovement
forward Black = up
forward White = down

-- | 右前方マス目リスト生成
--
-- >>> rightForward Black (F5, R5)
-- [(F4,R4),(F3,R3),(F2,R2),(F1,R1)]
--
-- >>> rightForward White (F5, R5)
-- [(F4,R6),(F3,R7),(F2,R8),(F1,R9)]
rightForward :: Color -> SquareMovement
rightForward Black = rightUp
rightForward White = rightDown

-- | 右後方マス目リスト生成
--
-- >>> rightBackward Black (F5, R5)
-- [(F4,R6),(F3,R7),(F2,R8),(F1,R9)]
--
-- >>> rightBackward White (F5, R5)
-- [(F4,R4),(F3,R3),(F2,R2),(F1,R1)]
rightBackward :: Color -> SquareMovement
rightBackward Black = rightDown
rightBackward White = rightUp

-- | 後方マス目リスト生成
--
-- >>> backward Black (F5, R5)
-- [(F5,R6),(F5,R7),(F5,R8),(F5,R9)]
--
-- >>> backward White (F5, R5)
-- [(F5,R4),(F5,R3),(F5,R2),(F5,R1)]
backward :: Color -> SquareMovement
backward Black = down
backward White = up

-- | 左後方マス目リスト生成
--
-- >>> leftBackward Black (F5, R5)
-- [(F6,R6),(F7,R7),(F8,R8),(F9,R9)]
--
-- >>> leftBackward White (F5, R5)
-- [(F6,R4),(F7,R3),(F8,R2),(F9,R1)]
leftBackward :: Color -> SquareMovement
leftBackward Black = leftDown
leftBackward White = leftUp

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

-- | 駒の打ち先を取得
droppable :: Color -> PieceType -> Board -> Droppable
droppable color pt board = Droppable
  (Set.difference squares (Map.keysSet (unBoard board)))
 where
  squares = Set.fromList [ (file, rank) | file <- fs, rank <- rs ]
  fs =
    if pt == Pawn then Set.toList $ withoutPawnFiles color board else [F9 .. F1]
  rs = ranks color pt

-- | 歩兵の居ない筋セット
withoutPawnFiles :: Color -> Board -> Set File
withoutPawnFiles color = Set.difference files . pawnFiles color
  where files = Set.fromList [F9 .. F1]

-- | 歩兵の筋セット
pawnFiles :: Color -> Board -> Set File
pawnFiles color board = Set.fromList $ fst <$> Map.keys pawns
 where
  pawns = Map.filterWithKey
    (\_ p -> pieceColor p == color && pieceType p == Pawn)
    (unBoard board)

-- | 駒の打ち先段リスト
ranks :: Color -> PieceType -> [Rank]
ranks Black Pawn   = [R2 .. R9]
ranks Black Lance  = [R2 .. R9]
ranks Black Knight = [R3 .. R9]
ranks White Pawn   = [R1 .. R8]
ranks White Lance  = [R1 .. R8]
ranks White Knight = [R1 .. R7]
ranks _     _      = [R1 .. R9]

-- | 駒成り
promote :: Promotion -> Piece -> Piece
promote promo piece = Piece color ppt
 where
  color = pieceColor piece
  ppt   = promotePieceType promo pt
  pt    = pieceType piece

-- | 駒成りを戻す
demote :: Piece -> Piece
demote piece = Piece color dpt
 where
  color = pieceColor piece
  dpt   = demotePieceType pt
  pt    = pieceType piece

-- | 成り駒の種類
promotePieceType :: Promotion -> PieceType -> PieceType
promotePieceType True Pawn   = PromotedPawn
promotePieceType True Lance  = PromotedLance
promotePieceType True Knight = PromotedKnight
promotePieceType True Silver = PromotedSilver
promotePieceType True Bishop = PromotedBishop
promotePieceType True Rook   = PromotedRook
promotePieceType _    pt     = pt

-- | 成り駒がなる前の駒の種類
demotePieceType :: PieceType -> PieceType
demotePieceType PromotedPawn   = Pawn
demotePieceType PromotedLance  = Lance
demotePieceType PromotedKnight = Knight
demotePieceType PromotedSilver = Silver
demotePieceType PromotedBishop = Bishop
demotePieceType PromotedRook   = Rook
demotePieceType pt             = pt

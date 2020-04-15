module ShogiX.Shogi.Stands
  ( empty
  , fromList
  , add
  , ShogiX.Shogi.Stands.drop
  , droppables
  )
where

import           RIO
import qualified RIO.Map                       as Map
import           ShogiX.Shogi.Types
import qualified ShogiX.Shogi.Piece            as Piece
import qualified ShogiX.Shogi.Stand            as Stand

-- | 空の駒台
empty :: Stands
empty = Stands Stand.empty Stand.empty

-- | リストから駒台を作成
fromList :: [(PieceType, Int)] -> [(PieceType, Int)] -> Stands
fromList b w = Stands (Stand.fromList b) (Stand.fromList w)

-- | 駒台に駒を追加
add :: Color -> Maybe PieceType -> Stands -> Stands
add _     Nothing   stands = stands
add color (Just pt) stands = setStand color newStand stands
  where newStand = Stand.add pt $ getStand color stands

-- | 駒台の駒を削除
drop :: Color -> PieceType -> Stands -> Maybe Stands
drop color pt stands = do
  newStand <- Stand.drop pt stand
  pure $ setStand color newStand stands
  where stand = getStand color stands

-- | 持ち駒の打ち先範囲を取得
droppables :: Color -> Board -> Stands -> Droppables
droppables color board stands = Droppables (Map.fromList ds)
 where
  ds  = [ (pt, Piece.droppable color pt board) | pt <- pts ]
  pts = Map.keys . unStand $ getStand color stands

-- | 先手後手の駒台から駒台取得
getStand :: Color -> Stands -> Stand
getStand Black = blackStand
getStand White = whiteStand

-- | 駒台更新
setStand :: Color -> Stand -> Stands -> Stands
setStand Black stand stands = stands { blackStand = stand }
setStand White stand stands = stands { whiteStand = stand }

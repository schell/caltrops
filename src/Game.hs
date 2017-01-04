module Game where

import Game.Common
import Data.Typeable
import Data.Text (Text)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Control.Monad

data Character = Character { characterId   :: Id
                           , characterName :: Text
                           } deriving (Typeable, Show, Eq)


gmLookup :: Int -> Int -> Int -> GameMap a -> Maybe a
gmLookup x y z xm = IM.lookup x xm >>= IM.lookup y >>= IM.lookup z

gmInsert :: Int -> Int -> Int -> a -> GameMap a -> GameMap a
gmInsert x y z a xm =
    let Just ym = msum [IM.lookup x xm, Just IM.empty]
        Just zm = msum [IM.lookup y ym, Just IM.empty]
    in IM.insert x (IM.insert y (IM.insert z a zm) ym) xm

type GameMap a = IntMap (IntMap (IntMap a))

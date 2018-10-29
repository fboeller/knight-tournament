module KnightTournament ( knight, game ) where

import Data.Foldable

data Knight = Knight
  String -- Name
  Int -- Health
  Int -- Damage
  deriving Show

knight :: String -> Int -> Int -> Knight
knight name health damage
  | health <= 0 = error $ "Knight " ++ name ++ " already dead!"
  | damage < 0 = error $ "Knight " ++ name ++ " can not deal negative damage!"
  | otherwise = Knight name health damage

-- The arena is a queue of knights
type Arena = [Knight]

-- The knight takes a hit and might fade into evanescence
takeHit :: Int -> Knight -> Maybe Knight
takeHit points (Knight name health damage)
  | points < health = Just $ Knight name (health - points) damage
  | otherwise = Nothing

-- The first knight hits the second one and will then be attached to the end.
-- If the health of the second knight is below zero, he is dead and will be removed.
-- If not, he is the next knight who gets the chance to hit.
turn :: Arena -> Arena
turn (knight@(Knight _ _ damage) : knight' : knights) =
  toList (takeHit damage knight') ++ knights ++ [knight]

game :: Arena -> Arena
game = until ((2>).length) turn

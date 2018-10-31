module KnightTournament ( Knight, game ) where

import Data.Foldable
import Data.Either
import Data.Maybe

data Knight = Knight
  String -- Name
  Int -- Health
  Int -- Damage
  deriving Show

type ValidationError = String

validateKnight :: Knight -> [ValidationError]
validateKnight (Knight name health damage) =
  map snd $ filter fst validations
  where validations = [
          (health <= 0, "Knight " ++ name ++ " already dead!"),
          (damage < 0, "Knight " ++ name ++ " can not deal negative damage!")]

validateArena :: Arena -> [ValidationError]
validateArena = concat.(map validateKnight)

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

game :: Arena -> Either [ValidationError] Knight
game arena = case validateArena arena of
  [] -> Right $ head $ until ((1==).length) turn arena
  errors -> Left errors

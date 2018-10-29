module KnightTournament ( knight, game ) where

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

takeHit :: Int -> Knight -> Knight
takeHit points (Knight name health damage) = Knight name (health - points) damage

-- The first knight hits the second one and will then be attached to the end.
-- If the health of the second knight is below zero, he is dead and will be removed.
-- If not, he is the next knight who gets the chance to hit.
turn :: Arena -> Arena
turn (knight@(Knight _ _ damage) : knight'@(Knight _ health' _) : knights)
  | damage >= health' = knights ++ [knight]
  | otherwise = [takeHit damage knight'] ++ knights ++ [knight]

game :: Arena -> Arena
game = until ((2>).length) turn

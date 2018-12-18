data Knight = Knight String Int Int deriving (Show, Read)

takeHit (Knight name damage health) points =
  map (Knight name damage) $ filter (>0) [health-points]

turn (knight@(Knight _ damage _) : knight' : knights) =
  takeHit knight' damage ++ knights ++ [knight]

main = getLine >>= putStrLn . show . head . until ((1==).length) turn . read

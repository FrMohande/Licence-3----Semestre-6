

import Graphics.Gloss

{- main :: IO ()
main = animate (InWindow "Dragon" (500, 500) (0, 0)) white (dragonAnime (50,250) (450,250))

dragonAnime :: RealFrac a => Point -> Point -> a -> Picture
dragonAnime a b t = Line (dragon a b !! (round t `mod` 20)) -}




-- Question 5
pointAintercaler :: Point -> Point -> Point
pointAintercaler (xA,yA) (xB,yB) = ( ((xA+xB)/2) + ((yB-yA)/2), ((yA+yB)/2) + ((xA-xB)/2) )

pasDragon :: Path -> Path
pasDragon [] = []
pasDragon [x] = [x]
pasDragon [x,y] = x : pointAintercaler x y : [y]
pasDragon (x:y:z:zs) = x : pointAintercaler x y : y : pointAintercaler z y : pasDragon (z:zs)

dragon :: Point -> Point -> [Path]
dragon x y = iterate pasDragon [x,y]

dragonOrdre :: Point -> Point -> Int -> Path
dragonOrdre a b n = dragonOrdre' [a,b] n

dragonOrdre' p 0 = p
dragonOrdre' p n
  |n==1 = pasDragon p
  |otherwise = dragonOrdre' (pasDragon p) (n-1)


main = animate (InWindow "Dragon" (500, 500) (0, 0)) white (dragonAnimeAlter (50,250) (450,250))

dragonAnimeAlter a b t = Line (dragonOrdre a b 15)

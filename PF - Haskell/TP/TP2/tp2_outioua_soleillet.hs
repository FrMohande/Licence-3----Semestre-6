-- Question 1
{-  tail [1] donne [] -}
alterne :: [a]->[a]
alterne [] = []
alterne [x] = [x]
alterne [x,y] = [x]
alterne (x:xs) = x : alterne (tail xs)


alterne' :: [a]->[a]
alterne' [] = []
alterne' [x] = [x]
alterne' [x,y] = [x]
alterne' (x:y:ys) = x : alterne (ys)

-- Question 2

combine :: (a -> b -> c) -> [a] -> [b] -> [c]
combine f [] [] = []
combine f (_:_) [] = []
combine f [] (_:_) = []
combine f (x:xs) (y:ys) = f x y : combine f xs ys

-- Question 3
{-  il faut additionner ces deux listes [1,3,3,1,0] + [0,1,3,3,1] pour obtenir la liste
[1,4,6,4,1]  -}
pasPascal :: [Integer] -> [Integer]
pasPascal [] = [1]
pasPascal l = zipWith (+) (0:l) (l ++ [0])

--Question 4 
pascal :: [[Integer]]
pascal = iterate pasPascal [1]

affiche10premier = take 10 pascal

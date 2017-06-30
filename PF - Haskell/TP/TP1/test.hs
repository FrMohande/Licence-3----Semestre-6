
somme'' n = sum [1..n]

longueur :: Num a => [t] -> a
longueur []     = 0
longueur (_:xs) = 1 + longueur xs

longueur' :: Num a =>[t] -> a
longueur' xs = case xs of
  [] -> 0
  _:xs -> 1 + longueur' xs


liste_sommes :: Num t =>[t]->[t]->[t]
liste_sommes [] [] = []
liste_sommes (x:xs) (y:ys) = x + y : liste_sommes xs ys

-- Exercice

--Version curryfiée
sommeDeXaY :: (Enum a, Num a) => a -> a -> a
sommeDeXaY x y = sum [x..y]

sommeDeXaY' :: Int-> Int -> Int
sommeDeXaY' x y = if (x == y) then x
                  else x + sommeDeXaY (x+1) y

-- Version décurryfiée

sommeDeXaY'' :: (Int,Int) -> Int
sommeDeXaY'' (x,y) = if (x==y) then x
                      else x + sommeDeXaY'' (x+1,y)


somme :: [Int]->Int
somme [] = 0
somme (x:xs) = x + somme xs

mylast :: [t] -> t
mylast (x:xs) = head (reverse (x:xs))

myinit :: [t]->[t]
myinit (x:xs) = take ((length (x:xs))-1) (x:xs)


(!!!) :: [t]->Int->t
(!!!) (x:_) 0 = x
(!!!) [] _ = error "Liste vide ou index out of bound"
(!!!) (x:xs) n = (!!!) xs (n-1)

(+++) :: [t] -> [t] -> [t]
(+++) [] [] = []
(+++) [] (y:ys) =  y : (+++) [] ys
(+++) (x:xs) [] = (x:xs)
(+++) (x:xs) (y:ys) =  x : (+++) xs (y:ys)



myconcat :: [[t]]->[t]
myconcat [] = []
myconcat (x:xs) = (+++) x (myconcat xs)

mymap :: (a->b)->[a]->[b]
mymap f [] = []
mymap f (x:xs) = f x : mymap f xs



{-
(!!) :: [t]->Int->t
x :: Int->t
x = (!!) l -
Ceci est une application partielle
-}


{- \_ est une fonction anonyme prends n'importe quoi et renvoie un 1
même chose que mettre toto _ = 1 -}

longueur'' :: [a]->Int
longueur'' l = somme (mymap (\_ -> 1) l)



funct :: (Eq a, Num a) => (t -> t) -> t -> a -> [t]
funct f x 0 = []
funct f x n = x : funct f (f x) (n-1)



funct' :: (a->a)->a->Int->[a]
funct' f x n = take n (iterate f x)


funct'' :: Num a => Int -> [a]
funct'' n = funct' (\x->x+1) 0 n

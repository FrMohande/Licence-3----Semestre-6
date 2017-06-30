import Graphics.Gloss

type Symbole  = Char
type Mot      = [Symbole]
type Axiome   = Mot
type Regles   = Symbole -> Mot
type LSysteme = [Mot]



-- Question 1
motSuivant :: Regles -> Mot -> Mot
motSuivant regles [] = []
motSuivant regles (x:xs) = regles x ++ motSuivant regles xs


{-
let a = ["abc,"def"]
concat :: [[a]]->[a]
concat a
résultat = "abcdef" -}

motSuivant' :: Regles -> Mot -> Mot
motSuivant' regles l = concat [regles x | x <- l]

{- concatMap  	(a -> [b]) -> [a] -> [b] -}
motSuivant'' :: Regles -> Mot -> Mot
motSuivant'' regles l = concatMap (regles) l


-- Question 2

maregles :: Symbole -> Mot
maregles 'F' = ['F', '-', 'F', '+', '+','F', '-', 'F']
maregles '-' = ['-']
maregles '+' = ['+']


-- Question 3
lsysteme :: Axiome -> Regles -> LSysteme
lsysteme axiome regles = [axiome] ++ (lsysteme (motSuivant'' regles axiome) regles)

ordreLsysteme :: Axiome -> Regles -> Int -> LSysteme
ordreLsysteme axiome regle n = take n (lsysteme axiome regle)


-- Tortue

type EtatTortue = (Point, Float)
type Config = (EtatTortue -- État initial de la tortue
              ,Float      -- Longueur initiale d’un pas
              ,Float      -- Facteur d’échelle
              ,Float      -- Angle pour les rotations de la tortue
              ,[Symbole]) -- Liste des symboles compris par la tortue


-- Question 4

etatInitial :: Config -> EtatTortue
etatInitial (x,_,_,_,_) = x


longueurPas :: Config -> Float
longueurPas (_,x,_,_,_) = x

facteurEchelle :: Config -> Float
facteurEchelle (_,_,x,_,_) = x

angle :: Config -> Float
angle (_,_,_,x,_) = x

symbolesTortue :: Config -> [Symbole]
symbolesTortue (_,_,_,_,x) = x





-- Question 5




avance :: Config -> EtatTortue -> EtatTortue
avance c ((x,y),cap) = ((x',y'),cap)
  where x' = x + pas * cos(cap)
        y' = y + pas * sin(cap)
        pas = longueurPas c


-- Question 6


tourneAGauche :: Config -> EtatTortue -> EtatTortue
tourneAGauche c (x,cap) = (x,cap')
  where cap' = cap + angle(c)


tourneADroite :: Config -> EtatTortue -> EtatTortue
tourneADroite c (x,cap) = (x,cap')
  where cap' = cap - angle(c)



-- Question 7EtatTortue



filtreSymbolesTortue :: Config -> Mot -> Mot
filtreSymbolesTortue c [] = []
filtreSymbolesTortue c (x:mots)
  | isInSymboleTortue (symbolesTortue c) x  = x : filtreSymbolesTortue c mots
  | otherwise = filtreSymbolesTortue c mots
  where isInSymboleTortue [] y = False
        isInSymboleTortue (s:ss) y
          | s == y = True
          | otherwise = isInSymboleTortue ss y

-- permet de tester notre fonctione filtreSymbolesTortue
configTest = (((4,5),5.0),2.0,2.0,2.0,['F','+','-'])


-- question 8

type EtatDessin = (EtatTortue, Path)
-- type EtatTortue = (Point, Float)
--type Path = [Point]
-- type Point = (Float, Float)


interpreteSymbole :: Config -> EtatDessin -> Symbole -> EtatDessin
interpreteSymbole c (etat,path) symbole
  | symbole == 'F' = (x,fst(x):path)
  | symbole == '-' = (y,fst(y):path)
  | symbole == '+' = (z,fst(z):path)
    where x = avance c etat
          y = tourneADroite c etat
          z = tourneAGauche c etat




  -- Question 9

{- Dans notre fonction interpreteSymbole le nouveau point est ajouté en tête du chemin

-}




-- Question 10

-- type Mot      = [Symbole]
-- line :: Path -> Picture

interpreteMot :: Config -> Mot -> Picture
interpreteMot c [] = line []
interpreteMot c (x:xs) = line (snd (interpreteMot' c (etatInitial c,[fst(etatInitial c)]) (x:xs) ) )


interpreteMot':: Config->EtatDessin->Mot->EtatDessin
interpreteMot' c etat [] = etat
interpreteMot' c etat (x:xs) = interpreteMot' c (interpreteSymbole c etat x) xs


-- Question 11

--lsysteme :: Axiome -> Regles -> LSysteme
-- type Regles   = Symbole -> Mot
-- type Mot      = [Symbole]
-- type Axiome   = Mot

-- ,Float      -- Longueur initiale d’un pas
-- ,Float      -- Facteur d’échelle
lsystemeAnime :: LSysteme -> Config -> Float -> Picture
lsystemeAnime lsysteme (a,b,c,d,e) f = interpreteMot (a,b*(c^(round f `mod` 7)),c,d,e) (lsysteme !! (round f `mod` 7))


vonKoch2 :: LSysteme
vonKoch2 = lsysteme "F++F++F++" regles
    where regles 'F' = "F-F++F-F"
          regles  s  = [s]

vonKoch2Anime :: Float -> Picture
vonKoch2Anime = lsystemeAnime vonKoch2 (((-400, -250), 0), 800, 1/3, pi/3, "F+-")

main = animate (InWindow "L-systeme" (1000, 1000) (0, 0)) white vonKoch2Anime


-- POUR FAIRE LE DESSSIN
-- ghc -Wall tortue.hs

{- Pour faire Flocon de von Koch
dessin = interpreteMot (((-150,0),0),100,1,pi/3,"F+-") "F+F--F+F"

main = display (InWindow "L-système" (1000, 1000) (0, 0)) white dessin -}

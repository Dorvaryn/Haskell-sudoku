import Data.List
type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Char

empty :: Value -> Bool
empty = (== '.')

boxsize :: Int
boxsize = 3

rows :: Matrix a -> [Row a]
rows = id

cols :: Matrix a -> [Row a]
cols = transpose

--transpose :: [[a]] -> [[a]]
--transpose [] = []
--transpose ([]:xs) = transpose xs
--transpose ((x:xs):xss) = (x:[h|(h:_)<-xss]) : transpose (xs:[t|(_:t)<-xss])

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

boxs :: Matrix a -> [Row a]
boxs = unpack . map cols . pack
       where pack = split . map split
             split = chop boxsize
             unpack = map concat . concat

valid :: Grid -> Bool
valid g = all nodups (rows g) &&
          all nodups (cols g) &&
          all nodups (boxs g)

nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x:xs) = all (x/=) xs && nodups xs

type Choices = [Value]
values :: [Value]
values = ['1','2','3','4','5','6','7','8','9']

choices :: Grid -> Matrix Choices
choices = map (map choice)
          where choice v = if empty v then values else [v]

single :: [a] -> Bool
single [_] = True
single _ = False

minus :: Eq a => [a] -> [a] -> [a]
minus xs ys = if single xs then xs else xs\\ys

singles = concat . filter single

reduce :: Eq a => [[a]] -> [[a]]
reduce xss = map (`minus` singles) xss
    where singles = concat (filter single xss)

prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxs . pruneBy rows . pruneBy cols
    where pruneBy f = f . map reduce . f

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [(y:ys)|y<-xs, ys<-cp xss]

collapse :: Matrix [a] -> [Matrix a]
collapse = cp . map cp

fix :: Eq a => ( a-> a) -> a -> a
fix f x = if x == x' then x else x'
    where x' = f x

solve :: Grid -> [Grid]
solve = filter valid . collapse . fix prune . choices

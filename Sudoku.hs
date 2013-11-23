module Sudoku
where

import Data.List
import Control.Monad.Fix

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
values = ['1'..'9']

choices :: Grid -> Matrix Choices
choices = map (map choice)
          where choice v = if empty v then values else [v]

single :: [a] -> Bool
single [_] = True
single _ = False

minus :: Eq a => [a] -> [a] -> [a]
xs `minus` ys = if single xs then xs else xs\\ys

singles = concat . filter single

reduce :: [Choices] -> [Choices]
reduce xss = [xs `minus` singles | xs <- xss]
    where singles = concat (filter single xss)

prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxs . pruneBy rows . pruneBy cols
    where pruneBy f = f . map reduce . f

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [y:ys|y<-xs, ys<-cp xss]

collapse :: Matrix [a] -> [Matrix a]
collapse = cp . map cp

fix' :: Eq a => ( a-> a) -> a -> a
fix' process = fix (\rec c -> let processed = process c in if c == processed then c else rec processed) 

solve :: Grid -> [Grid]
solve = filter valid . collapse . fix' prune . choices

consistent :: [Choices] -> Bool
consistent = nodups . concat . filter single

safe :: Matrix Choices -> Bool
safe m = all consistent (rows m)&&
         all consistent (cols m)&&
         all consistent (boxs m)

complete :: Matrix Choices -> Bool
complete = all (all single)

expand :: Matrix Choices -> [Matrix Choices]
expand m = [rs1 ++ [r1 ++ [x]:r2] ++ rs2 | x <- xs]
            where (rs1, r:rs2) = break (any (not . single)) m
                  (r1, xs:r2) = break (not . single) r

search :: Matrix Choices -> [Grid]
search m
    | not (safe m) = []
    | complete m = collapse m
    | otherwise = [g | m' <- expand m,
                       g <- search (prune m')]

solveFast :: Grid -> [Grid]
solveFast = search . prune . choices

easy :: Grid
easy = ["2....1.38",
        "........5",
        ".7...6...",
        ".......13",
        ".981..257",
        "31....8..",
        "9..8...2.",
        ".5..69784",
        "4..25...."]

try :: Grid
try =  [".9.7..86.",
        ".31..5.2.",
        "8.6......",
        "..7.5...6",
        "...3.7...",
        "5...1.7..",
        "......1.9",
        ".2.6..35.",
        ".54..8.7."]

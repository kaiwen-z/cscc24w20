module MonadRandom where

import Control.Applicative (liftA2)
import Control.Applicative

import MonadRandomDef

instance Functor DecisionTree where
  fmap f (Tip a) = (Tip (f a))
  fmap f t = t >>= \x -> return (f x)

instance Applicative DecisionTree where
  pure = Tip
  liftA2 op t1 t2 = t1 >>= \x1 -> t2 >>= \x2 -> return (op x1 x2)
  
instance Monad DecisionTree where
  return = Tip
  Tip a >>= f = f a
  Choose val lt rt >>= f = Choose val (lt >>= f) (rt >>= f)

instance MonadRandom DecisionTree where
  choose pval ltree rtree = Choose pval (ltree >>= return) (rtree >>= return)

-- Calculate the expected value of the DecisionTree
expectedValue :: Fractional q => (t -> q) -> DecisionTree t -> q
expectedValue f (Tip char) = (f char)
expectedValue f (Choose val (ltree) (rtree)) = (((expectedValue f ltree) * (realToFrac val)) + ((expectedValue f rtree) * (1 - (realToFrac val))))

-- Probability function provided in startercode
probability :: Fractional q => (t -> Bool) -> DecisionTree t -> q
probability pred t = expectedValue bernoulli t
  where
    bernoulli a | pred a = 1
                | otherwise = 0

-- Create a dtree with uniform distribution
uniform :: MonadRandom m => [a] -> m a
uniform (x:xs)
  | n == 1 = pure x -- If just Tip
  | otherwise = choose pval (uniform rlist) (uniform llist) -- Recurse over each sublist
  where
    rlist = fst (split (x:xs))
    llist = snd (split (x:xs))
    n = length (x:xs)
    pval = toRational ((toRational (length rlist)) / (toRational (length (x:xs))))

-- Split List function from Lab 2
split :: [a] -> ([a], [a])
split [] = ([], [])
split [x] = ([x], [])
split (x:y:xys) = (x:xs, y:ys)
    where (xs, ys) = split xys

-- HERE IS THE BEST ANSWER FOR HANGMAN
realHangman :: MonadRandom m => String -> Int -> String -> m WinLose
realHangman word numtrails alphabet = uniform alphabet >>= \a -> realHangman (delete a word) (numtrails - 1) (delete a alphabet)


-- Example input:
-- hangman "lol" 2 "lcbo" :: DecisionTree WinLose
-- Algorithm: (Generates the smallest possible correct tree)
-- Generate combination of all guesses of length turns (not permutation, since order of guesses dont matter)
-- Using combination instead of permutations also shrinks the tree size exponentially (10 choose 5) = 252, (10 pick 5) = ~30,000
-- Filter out duplicate values from the answer string, just makes it a bit easier to check for Win or Lose
-- Replace winning guesses with Win and losing with Lose in the combination list
-- Build combination tree into uniform distribution, as all guesses are equally likely
-- Since the final tree is generated using uniform, the tree is also perfectly balanced, as all things should be
hangman :: MonadRandom m => String -> Int -> String -> m WinLose
hangman win turns guess = build (replaceWinLose (unique win) (combinate turns guess))

-- From a list of Win Lose, build uniform distribution tree
build :: MonadRandom m => [WinLose] -> m WinLose
build list = uniform list

-- replace correct guess with Win and others with Lose
replaceWinLose :: String -> [String] -> [WinLose]
replaceWinLose win [] = []
replaceWinLose win (x:xs)
  | (checkString x win) = Win : replaceWinLose win xs
  | otherwise = Lose : replaceWinLose win xs

-- combinate and distribute are simplified from haskell built in Data.permute to fit the use case
-- The functions were also re-written to allow for finding strings of length n instead of the entire lenght of the string
-- and also modified to find combinations instead of permutations
-- Generate combination of strings of length n
combinate :: Int -> String -> [String]
combinate _ [] = []
combinate 0 (x:xs) = []
combinate 1 (x:xs) = (toString x) : combinate 1 xs
combinate 2 (x:xs) = map (\f -> x:[f]) xs ++ combinate 2 xs
combinate n (x:xs)
  | n <= max = foldr (++) [] (map (distribute [] x) (combinate (n-1) xs))
  | otherwise = combinate max (x:xs)
  where
    max = length (x:xs)

-- Aid in combinations by moving head value through list
-- example: [] a [b,c] yeilds abc, bac, bca
distribute :: [a] -> a -> [a] -> [[a]]
distribute leftover char [] = [leftover ++ [char]]
distribute leftover char (x:xs) = (leftover ++ (char:x:xs)) : (distribute (leftover ++ [x]) char xs)

-- Changes char to string type
toString :: Char -> String
toString c = [c]

-- Check if one string contains all chars of another, strings need to
-- swap positions if num turns > len guesses
checkString :: Eq a => [a] -> [a] -> Bool
checkString a b
  | sa >= sb = containString a b
  | otherwise = containString b a
  where
    sa = length a
    sb = length b

-- Checks if a string contains all the chars of another string
containString :: Eq a => [a] -> [a] -> Bool
containString [] [] = True
containString _ [] = True
containString [] _ = True
containString x (y:ys) = contains x y && containString x ys

-- Return a list with duplicate values removed
unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs)
  | contains xs x = unique xs
  | otherwise = x : unique xs

-- Returns True if a string contains a char
contains :: (Eq a) => [a] -> a -> Bool
contains [] _ = False
contains (x:xs) a
  | a == x = True
  | otherwise = contains xs a

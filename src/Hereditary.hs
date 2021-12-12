module Hereditary (Set, (∈), (⊆),
    empty, power, true, replace, specification, union,
    Encodable, fromSet) where
import Data.Function (on)
import Data.List (isSubsequenceOf, subsequences, nub, sort)
import Control.Monad (filterM)

-- The following is an elegant but *very* inefficient version using
-- von Neumann encoding.
{-
newtype Set = FromNat Integer deriving (Eq, Read)
toNat :: Set -> Integer
toNat (FromNat x) = x

(∈) :: Set -> Set -> Bool
FromNat n ∈ FromNat m
    = 1 == (m `div` (2 ^ n)) `mod` 2

toBinary :: Integer -> [Bool]
toBinary 0 = []
toBinary n = let (n', r) = n `divMod` 2 in
    (r==1) : toBinary n'

elements :: Set -> [Set]
elements (FromNat n) = map (FromNat . fst) $ filter snd $ zip [0..] $ toBinary n

fromList :: [Set] -> Set
fromList = FromNat . sum . map ((2^) . toNat)
-}

newtype Set = FromList [Set] deriving (Eq, Read, Ord)
elements :: Set -> [Set]
elements (FromList e) = e

(∈) :: Set -> Set -> Bool
x ∈ y = x `elem` elements y

-- The following does not involve internal representations.
-- So the two versions both work.
fromList = FromList . sort

instance Show Set where
    show x = "{" ++ unwords (map show (elements x)) ++ "}"

(⊆) :: Set -> Set -> Bool
(⊆) = isSubsequenceOf `on` elements

empty :: Set
empty = fromList []

power :: Set -> Set
power = fromList . map fromList . subsequences . elements

union :: Set -> Set
union = fromList . nub . concatMap elements . elements

true :: Set
true = power empty

replace :: (Monad m) => (Set -> m Set) -> (Set -> m Set)
replace f x = fromList . nub <$> mapM f (elements x)

specification :: (Monad m) => (Set -> m Bool) -> Set -> m Set
specification p x = fromList <$> filterM p (elements x)

-- Pretty printer.
class Encodable a where
    fromSet :: Set -> a

instance Encodable Int where
    fromSet = length . elements

instance (Encodable a, Encodable b) => Encodable (a, b) where
    -- {{x}, {x, y}}
    fromSet x = let elems = elements x in
        if length elems == 1 then -- singleton
            let u = head (elements (head elems)) in (fromSet u, fromSet u)
        else
            let [u, v] = elems in
            let [x1] = elements u in
            let [x2] = filter (/= x1) (elements v) in
                (fromSet x1, fromSet x2)

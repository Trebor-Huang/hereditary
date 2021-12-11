module Hereditary (Set, (∈), (⊆),
    empty, power, true, replace, specification, union,
    Encodable, fromSet) where
import Data.Function (on)
import Data.List (isSubsequenceOf, subsequences, nub)
import Control.Monad (filterM)

newtype Set = FromNat Integer deriving (Eq, Read)
toNat :: Set -> Integer
toNat (FromNat x) = x

imply :: Bool -> Bool -> Bool
imply a b = b || not a

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

instance Show Set where
    show x = "{" ++ unwords (map show (elements x)) ++ "}"

(⊆) :: Set -> Set -> Bool
(⊆) = isSubsequenceOf `on` elements

empty :: Set
empty = FromNat 0

-- | Ad-hoc bounded comprehension.
comprehension :: Integer -> (Set -> Bool) -> Set
comprehension bound pred = FromNat $ sum $ map (2^)
    $ filter (pred . FromNat) [0..bound]

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

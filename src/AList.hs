{-#LANGUAGE ViewPatterns #-}
module AList where

import Prelude hiding (lookup, elem)
import qualified Prelude as P
import Data.List hiding (elem)


-- normally, a context G extended with x:t is written G,x:t
-- for efficiency however, we store the bindings in reverse order
newtype AList k v = A [(k, v)]

empty :: AList k v
empty = A []

lookup :: (Eq k) => AList k v -> k -> Maybe v
lookup (A xs) k = P.lookup k xs

update :: (Eq k) => AList k v -> k -> v -> AList k v
update (A xs) k v = A $ (k, v):xs

elem :: (Eq k) => k -> AList k v -> Bool
elem _ (A []) = False
elem k0 (A ((k,_):rest)) = k == k0 || k0 `elem` A rest

domain :: (Eq k) => AList k v -> [k]
domain (A xs) = reverse . nub $ fst <$> xs


_clean :: (Eq k) => AList k v -> AList k v
_clean (A xs) = A $ go [] xs
    where
    go seen [] = []
    go seen ((k, v):rest)
        | k `P.elem` seen = go seen rest
        | otherwise = (k, v) : go (k:seen) rest

fromList :: (Eq k) => [(k, v)] -> AList k v
fromList = _clean . A . reverse

toList :: (Eq k) => AList k v -> [(k, v)]
toList (_clean -> A xs) = reverse xs


instance (Eq k, Show k, Show v) => Show (AList k v) where
    show = show . toList
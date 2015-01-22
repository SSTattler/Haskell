import Test.QuickCheck
import Data.List


--
-- quickSort 
--
quickSort :: (Ord a) => [a] -> [a]
----------------------------------
quickSort []       = []
quickSort (x:rest) = 
   quickSort [ls | ls <- rest, ls < x] ++ [x] ++ quickSort [ge | ge <- rest, ge >= x]

--OR:
--    quickSort [ls | ls <- rest, ls < x] ++ x : quickSort [ge | ge <- rest, ge >= x]
--


--
-- Test: quickSort
--

prop_idempotent xs = quickSort (quickSort xs) == quickSort xs

prop_minimum xs    = not (null xs) ==> head (quickSort xs) == minimum xs
prop_maximum xs    = not (null xs) ==> last (quickSort xs) == maximum xs

prop_ordered xs    = ordered (quickSort xs)
                        where ordered []       = True
                              ordered [x]      = True
                              ordered (x:y:xs) = x <= y && ordered (y:xs)

prop_permutation xs = permutation xs (quickSort xs)
   where 
      permutation xs ys = null (xs \\ ys) && null (ys \\ xs)

prop_append xs ys = 
   not (null xs) ==> 
      not (null ys) ==> 
         head (quickSort (xs ++ ys)) == min (minimum xs) (minimum ys)

prop_sort_model xs = sort xs == quickSort xs

-- prop_idempotent []
-- 
-- quickCheck (prop_idempotent :: [Integer] -> Bool)
-- verboseCheck (prop_idempotent :: [Integer] -> Bool)

test = do
         quickCheck (prop_idempotent   :: [Integer] -> Bool)
         quickCheck (prop_ordered      :: [Integer] -> Bool)
         quickCheck (prop_permutation  :: [Integer] -> Bool)
         quickCheck (prop_minimum      :: [Integer] -> Property)
         quickCheck (prop_maximum      :: [Integer] -> Property)
         quickCheck (prop_append       :: [Integer] -> [Integer] -> Property)
         quickCheck (prop_sort_model   :: [Integer] -> Bool)
         -- Try Rational               
         quickCheck (prop_sort_model   :: [Rational] -> Bool)
         -- Try Float                  
         quickCheck (prop_sort_model   :: [Float] -> Bool)
         -- Now String                 
         quickCheck (prop_sort_model   :: [String] -> Bool)
         -- Try verboseCheck 
         verboseCheck (prop_sort_model :: [Integer] -> Bool)
         -- Now Ord - it does not work the type is failed - it is ambiguous
         -- quickCheck (prop_idempotent  :: [ord] -> Bool)

main = test
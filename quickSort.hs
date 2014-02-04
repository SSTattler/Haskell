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
 

prop_idempotent xs = quickSort (quickSort xs) == quickSort xs

prop_minimum xs    = not (null xs) ==> head (quickSort xs) == minimum xs

prop_ordered xs    = ordered (quickSort xs)
                        where ordered []       = True
                              ordered [x]      = True
                              ordered (x:y:xs) = x <= y && ordered (y:xs)

prop_permutation xs = permutation xs (quickSort xs)
   where permutation xs ys = null (xs \\ ys) && null (ys \\ xs)

prop_maximum xs = not (null xs) ==> last (quickSort xs) == maximum xs

prop_append xs ys = 
   not (null xs) ==> 
      not (null ys) ==> 
         head (quickSort (xs ++ ys)) == min (minimum xs) (minimum ys)




-- prop_idempotent []
-- 
-- quickCheck (prop_idempotent :: [Integer] -> Bool)
-- verboseCheck (prop_idempotent :: [Integer] -> Bool)

-- :l * -v
-- :l *
-- :l *Test -v
-- :q
-- :q
-- ccccccc


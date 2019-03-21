minList :: (Ord a) => [a] -> a
minList (x:xs) = foldl min x xs

maxList :: (Ord a) => [a] -> a
maxList (x:xs) = foldl max x xs

remove :: (Eq a) => a -> [a] -> [a]
remove e [] = []
remove e (x:xs)
  | e == x = xs
  | e /= x = x : remove e xs

removeMin :: (Ord a, Eq a) => [a] -> [a]
removeMin xs = remove (minList xs) xs

removeMax :: (Ord a, Eq a) => [a] -> [a]
removeMax xs = remove (maxList xs) xs

removeExtremum :: (Ord a, Eq a) => [a] -> [a]
removeExtremum xs = removeMin (removeMax xs)

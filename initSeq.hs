remove :: (Eq a) => a -> [a] -> [a]
remove e [] = []
remove e (x:xs)
  | e == x = xs
  | e /= x = x : remove e xs

inits' :: (Eq a) => [a] -> [[a]]
inits' [] = []
inits' xs = init xs : inits' (remove (last xs) xs)

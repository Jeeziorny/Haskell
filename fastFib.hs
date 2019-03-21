fastFib :: (Eq a, Num a) => a -> a -> a -> a
fastFib prev res 0 = res
fastFib prev res n = fastFib res (prev + res) (n-1)

somaETriplica :: Int -> (Int -> Int)
somaETriplica x y = (x + y) * 3

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

appSnd :: (a, b) -> (b -> c) -> (a, c)
appSnd (x, y) f = (x, f y) 

any1 :: (a -> Bool) -> [a] -> Bool
any1 f [] = False
any1 f (x:xs) = f x || any1 f xs

dobra :: Int -> Int
dobra x = 2 * x

quadSum:: Int -> Int -> Int
quadSum x y = (x + y) ^ 2

app2 :: (Int -> Int) -> Int -> Int
app2 f x = f (f x)

soma :: Int -> Int -> Int
soma x y = x + y

fib :: Int -> Int
fib x = if x == 0 then 0 else if x == 1 then 1 else fib (x - 1) + fib (x - 2)

hello :: String
hello = "Hello World"


(%) :: Int -> Int -> Int
x % y = x `mod` y

(&) :: a -> (a -> b) -> b
x & f = f x

bateu :: String -> String -> String
bateu x y = "O " ++ x ++ " bateu no " ++ y

(^///^) :: String -> String -> String
x ^///^ y = x ++ " ^///^ " ++ y

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

xor2 :: Bool -> Bool -> Bool
xor2 x y = (x || y) && not (x && y)

xor3 :: Bool -> Bool -> Bool
xor3 x y = x /= y

avaliacao :: Float -> Float -> String
avaliacao x y
    | media == 10 = "Parabens por nada"
    | media == 0 = "Voce e um lixo"
    | media >= 7 = "Aprovado"
    | otherwise = "Reprovado"
    where media = (x + y) / 2
safeDiv :: Float -> Float -> Maybe Float
safeDiv _ 0 = Nothing
safeDiv x y = Just (x / y)

data Pessoa =
    Fisica Int String String
    | Juridica String String
    deriving ()

getNome :: Pessoa -> String
getNome (Fisica _ nome _) = nome
getNome (Juridica nome _) = nome

instance Eq Pessoa where
    p1 == p2 = getNome p1 == getNome p2

instance Show Pessoa where
    show = getNome

class Ifable a where
    if' :: a -> Int -> Int -> Int

instance Ifable Int where
    if' x y z = if x > 0 then y else z

instance Ifable [a] where
    if' [] x y = y
    if' _ x y = x

data List a = 
    Empty
    | a :> List a
    deriving (Eq, Show)
infixr 5 :>


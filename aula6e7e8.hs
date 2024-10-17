{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
import Data.Char (toUpper)
import System.Random (randomRIO)
data Vec3 a = Vec3 a a a

instance Show a => Show (Vec3 a) where
    show (Vec3 x y z) = "(" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"


hasElement :: Eq a => a -> Vec3 a -> Bool
hasElement a (Vec3 x y z) = a == x || a == y || a == z


--data Metastring = Meta String Int Int

data Meta a = Meta{content :: a, nlines :: Int, nwords :: Int} deriving Show

metaFromString :: String -> Meta String
metaFromString str = Meta str (length $ lines str) (length $ words str)

instance Semigroup Int where
    (<>) = (+)

instance Semigroup a => Semigroup (Meta a) where
    (Meta s1 l1 w1) <> (Meta s2 l2 w2) = Meta (s1 <> s2) (l1 + l2) (w1 + w2)

instance Functor Meta where
    fmap f (Meta c l w ) = Meta (f c) l w

toUpperString :: String -> String
toUpperString str = fmap toUpper str

somaRandom :: Int -> IO Int
somaRandom n = do
    x <- randomRIO (1, 100)
    return (n + x)

main :: IO ()
main = do
    str <- readFile "teste.txt"
    let meuMeta = metaFromString str
    writeFile "meta.test" (show $ fmap toUpperString meuMeta)


listTest :: [Int]
listTest = do
    x <- [1, -1]
    y <- [1..10]
    return (x + y)

safeDiv :: Float -> Float -> Maybe Float
safeDiv x 0 = Nothing
safeDiv x y = Just $ x / y

div3 :: Float -> Float -> Float -> Maybe Float
div3 x y z = do
    xDivY <- safeDiv x y
    safeDiv xDivY z

module Lib
    ( Optional(Nada, Only), someFunc
    ) where


import Data.Monoid

data Optional a = Nada | Only a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
    (<>) a b = case (a,b) of
        (Nada, x) -> x
        (x, Nada) -> x
        (Only x, Only y) -> Only $ x <> y

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

someFunc :: IO ()
someFunc = putStrLn "someFunc"

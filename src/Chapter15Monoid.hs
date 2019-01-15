module Chapter15Monoid
    ( Trivial(..), Optional(..), Identity(..), Combine(..), Two(..), First'(..), FirstMappend,
    Mem(..),
    mainMonoid
    ) where


import Data.Monoid

-- Most of the exercises of chapter 15

data Optional a = Nada | Only a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
    (<>) a b = case (a,b) of
        (Nada, x) -> x
        (x, Nada) -> x
        (Only x, Only y) -> Only $ x <> y

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
           -> Adverb
           -> Noun
           -> Adjective
           -> String
madlibbin' e adv noun adj =
  mconcat [e, "! he said ", adv,  " as he jumped into his car " ,noun, " and drove off with his " ,adj, " wife."]



newtype First' a =
  First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) (First' (Only x)) _ = First' (Only x)
  (<>) (First' Nada) (First' (Only x)) = First' (Only x)
  (<>) _ _ = First' Nada

instance Monoid (First' a) where
  mempty = First' Nada



firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
 _ <> _ = Trivial



data Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
 Identity a <> Identity b = Identity ( a <> b)



data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two a b) (Two c d) = Two (a <> c) (b <> d)

instance (Monoid a, Monoid b) => Monoid  (Two a b) where
  mempty = Two (mempty ) (mempty )

newtype Combine a b = Combine { unCombine :: (a -> b) }


instance (Semigroup b) => Semigroup (Combine a b) where
   (Combine f) <> (Combine g) = Combine (\x -> (f x) <> (g x))



-- Exercise 8

newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance Semigroup a => Semigroup (Mem s a) where
   m1 <> m2 = Mem ( \s0 -> let (a1,s1) = runMem m1 s0
                               (a2,s2) = runMem m2 s1
                          in (a1 <> a2, s2) )


instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)

f' = Mem $ \s -> ("hi", s + 1)

mainMem :: IO ()
mainMem = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0


mainMonoid :: IO ()
mainMonoid = mainMem




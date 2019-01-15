module Chapter15Monoid
    ( madlib, Trivial(..), Identity(..), Combine(..), Two(..), First'(..), FirstMappend
    ) where


import Data.Monoid
import Lib

-- Most of the exercises of chapter 15

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


instance Show (Combine a b) where
   show _ = "fun"

instance (Semigroup b) => Semigroup (Combine a b) where
   (Combine f) <> (Combine g) = Combine (\x -> (f x) <> (g x))




newtype Mem s a = Mem { runMem :: s -> (a,s) }


madlib :: IO ()
madlib = putStrLn $ madlibbin' "Damn" "quickly" "Lada" "crazy"


import Test.QuickCheck
import Test.Hspec
import Lib
import Chapter15Monoid

import Data.Monoid
import Test.QuickCheck.Arbitrary

{-# LANGUAGE TypeSynonymInstances,
              FlexibleInstances ,
              FlexibleContexts #-}


monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

combineAssoc :: (Eq b, Semigroup b) => a -> (Combine a b) -> (Combine a b) -> (Combine a b) -> Bool
combineAssoc a c1 c2 c3 = unCombine (c1 <> (c2 <> c3))  a  == unCombine ((c1 <> c2) <> c3)  a

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    frequency [ (1, return $ Identity a)  ]

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    frequency [ (3, return $ First' (Only a)),
                (1, return $ First' Nada) ]

instance (Arbitrary a, Arbitrary b )=> Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return $ Two a b)  ]

-- see https://kseo.github.io/posts/2016-12-14-how-quick-check-generate-random-functions.html

instance CoArbitrary (Combine a b) where
  coarbitrary (Combine f) = variant 0

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return $ Combine f

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: First' String -> Bool)
  quickCheck (monoidRightIdentity :: First' String -> Bool)
  quickCheck (semigroupAssoc :: Trivial -> Trivial -> Trivial -> Bool)
  quickCheck (semigroupAssoc :: Identity String -> Identity String -> Identity String -> Bool)
  quickCheck (monoidAssoc :: Two String String -> Two String String -> Two String String -> Bool)
  quickCheck (combineAssoc :: Int -> Combine Int (Sum Int) -> Combine Int (Sum Int) -> Combine Int (Sum Int) -> Bool)

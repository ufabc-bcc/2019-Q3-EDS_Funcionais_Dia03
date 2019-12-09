-- Emilio Francesquini <e.francesquini@ufabc.edu.br>
-- CC-BY-SA
-- 11/2019

module Main where

import FingerTree

import Test.Tasty
import Test.Tasty.QuickCheck as QC -- Random testing

import Data.List

ntests :: Int
ntests = 1000

tests :: TestTree
tests = testGroup "FingerTrees" [fingerTreeTests]


fingerTreeTests :: TestTree
fingerTreeTests = testGroup "FingerTrees" [
    dequeProperties
  , seqProperties
  , heapProperties]


dequeProperties :: TestTree
dequeProperties =
  testGroup "Deque" [
      testProp "Creation" creationProp
    , testProp "Concatenation" concatProp
    , testProp "Cons" consProp
    , testProp "Snoc" snocProp]
  where
    testProp desc prop = QC.testProperty desc (withMaxSuccess ntests prop)

creationProp :: [Int] -> Bool
creationProp xs =
  toList (fromList xs :: Deque Int) == xs

concatProp :: [Int] -> [Int] -> Bool
concatProp xs ys =
  toList (dxs `catenate` dys) == (xs ++ ys)
  where
    dxs = fromList xs :: Deque Int
    dys = fromList ys

consProp :: Int -> [Int] -> Bool
consProp x xs =
  toList (cons x dxs) == (x:xs)
  where
    dxs = fromList xs :: Deque Int

snocProp :: Int -> [Int] -> Bool
snocProp x xs =
  toList (dxs `snoc` x) == (xs ++ [x])
  where
    dxs = fromList xs :: Deque Int

seqProperties :: TestTree
seqProperties =
  testGroup "Sequence" [
      testProp "Creation" seqCreationProp
    , testProp "Concatenation" seqConcatProp
    , testProp "Length" seqLengthProp
    , testProp "Length 2" seqLength2Prop
    , testProp "splitAt" seqSplitAtProp
    , testProp "(!)" seqIndexProp]
  where
    testProp desc prop = QC.testProperty desc (withMaxSuccess ntests prop)

seqCreationProp :: [Int] -> Bool
seqCreationProp xs =
  toList s == xs
  where
    s = fromList xs :: Seq Int

seqConcatProp :: [Int] -> [Int] -> Bool
seqConcatProp xs ys =
  toList (sxs `catenate` sys) == (xs ++ ys)
  where
    sxs = fromList xs :: Seq Int
    sys = fromList ys

seqLengthProp :: [Int] -> Bool
seqLengthProp xs =
  seqLength sxs == length xs
  where
    sxs = fromList xs :: Seq Int

seqLength2Prop :: [Int] -> Int -> Bool
seqLength2Prop xs x =
  seqLength (snoc sxs x) == length xs + 1
  where
    sxs = fromList xs :: Seq Int

seqSplitAtProp :: [Int] -> Int -> Bool
seqSplitAtProp xs i =
  (toList sl, toList sr) == splitAt i xs
  where
    (sl, sr) = seqSplitAt i (fromList xs)

seqIndexProp :: [Int] -> NonNegative Int -> Property
seqIndexProp xs i0 =
  i < length xs ==> s ! i == xs !! i
  where
    i = getNonNegative i0
    s = fromList xs :: Seq Int

heapProperties :: TestTree
heapProperties =
  testGroup "Min Heap" [
      testProp "Happy Day" heapHappyDay
      ]
  where
    testProp desc prop = QC.testProperty desc (withMaxSuccess ntests prop)

heapHappyDay :: [Int] -> Bool
heapHappyDay xs =
  hList == sort xs
  where
    push h v = heapPush h v v
    heap = foldl push (Heap Empty) xs
    hList = heapToOrderedList heap

-- Este módulo contém alguns testes bem básicos sobre as estruturas de
-- dados dadas em aula. Estes testes estão longe de representar uma
-- cobertura completa, mas servem como uma boa base para criação de
-- novos testes.

main :: IO ()
main = defaultMain tests

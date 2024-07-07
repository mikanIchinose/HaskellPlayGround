{-# OPTIONS -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

import Prelude hiding (all)

-- パターンマッチ
-- 引数の構造での場合分け

-- 7か7以外かでのパターンマッチ
lucky :: Int -> String
lucky 7 = "Lucky Number Seven!!"
lucky _ = "Sorry, you're out of luck, pal"

-- 階上計算
-- パターンマッチによって階上の数学的定義みたいなかんじで定義できる
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- 福利計算
welfare :: Double -> Integer -> Double
welfare x n = x * (1.03 ^ n)

-- タプルのパターンマッチ
addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

-- リストのパターンマッチ
-- let xs = [(1,3),(4,3),(2,4),(5,3),(5,6),(3,1)]
-- [a+b | (a,b) <- xs]
head' :: [a] -> a
-- 空のリストにマッチ
head' [] = error "Can't call head on an empty list, dummy!"
head' (x : _) = x

tell :: (Show a) => [a] -> String
-- 空のリストにマッチ
tell [] = "The list is empty"
-- 長さ1のリストにマッチ
tell (x : []) = "The list has one element: " ++ show x
-- 長さ2のリストにマッチ
tell (x : y : []) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x : y : _) = "The list is long. The first two elements are: " ++ show x ++ " and " ++ show y

-- as pattern
firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
-- パターンマッチに利用した対象にも参照できる
firstLetter all@(x : _) = "The first letter of " ++ all ++ " is " ++ [x]

-- Guard
-- 引数の値で場合分けする
bmiTell :: Double -> String
bmiTell bmi
  | bmi <= 18.5 = "You're underweight, you emo, you!"
  | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"

bmiTell2 :: Double -> Double -> String
bmiTell2 weight height
  | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
  | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"

max' :: (Ord a) => a -> a -> a
max' a b
  | a <= b = b
  | otherwise = a

compare' :: (Ord a) => a -> a -> Ordering
a `compare'` b
  | a == b = EQ
  | a <= b = LT
  | otherwise = GT

-- where
-- 計算結果を変数に格納するためのキーワード
bmiTell3 :: Double -> Double -> String
bmiTell3 weight height
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= fat = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
  where
    bmi = weight / height ^ 2
    skinny = 18.5
    normal = 25.0
    fat = 30.0

-- パターンマッチをまたいで変数が使いたいならグローバルに設定する必要がある
niceGreeting :: String
niceGreeting = "Hello! So veri nice to meet you,"

badGreeting :: String
badGreeting = "Oh! Pfft. It's you."

greet :: String -> String
greet "Juan" = niceGreeting ++ " Juan!"
greet "Fernando" = niceGreeting ++ " Fernando!"
greet name = badGreeting ++ " " ++ name

-- where - pattern match
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where
    (f : _) = firstname
    (l : _) = lastname

-- where - function
calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where
    bmi weight height = weight / height ^ 2

-- let expression
-- whereよりも局所的な変数束縛
-- let bindings in expression
cylinder :: Double -> Double -> Double
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
   in sideArea + 2 * topArea

-- let - list comprehension
calcBmis2 :: [(Double, Double)] -> [Double]
calcBmis2 xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi > 25.0]

-- case expression
-- パターンマッチをどこにでも置く
describeList :: [a] -> String
describeList ls =
  "The list is"
    ++ case ls of
      [] -> "empty."
      [_] -> "a singleton list."
      _ -> "a longer list."

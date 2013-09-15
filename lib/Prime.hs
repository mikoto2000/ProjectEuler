module Prime
( isPrime         -- 素数判定
, getPrimeNumbers -- 素数のリスト取得
, nextPrime       -- 次の素数取得
, primeFactor     -- 素因数分解
) where

-- 試し割り法で素数を判定する。
-- 引数で渡された整数値が素数であれば True を返す
isPrime :: Integer -> Bool
isPrime t
    | t <= 0 = False
    | t == 1 = False
    | otherwise = isPrime' t 2

-- 第一引数で渡された整数値が素数であれば True を返す
-- 第一引数 : 素数かどうか判定したい整数
-- 第二引数 : 今篩っている数値
isPrime' :: Integer -> Integer -> Bool
isPrime' t n -- t:target, n:number
    | t == n  = True          -- t までのすべての数で割り切れなかった
    | om == 0 = False         -- 割り切れた、つまり素数でない
    | om > 0  = isPrime' t (n+1) -- 割り切れなかったけれどまだ次がある
    | otherwise = False -- その他
    where om = mod t n


-- s から e までの isPrime の結果を取得
getPrimeResults :: Integer -> Integer -> [Bool]
getPrimeResults s e = map (isPrime) [s..e]


-- 対象の数と isPrime の結果をタプルに格納する
getPrimeResults' :: Integer -> Integer -> [(Bool, Integer)]
getPrimeResults' s e = zip (getPrimeResults s e) [s..e]

-- 素数のタプルだけ取得
getPrimeResults'' :: Integer -> Integer -> [(Bool, Integer)]
getPrimeResults'' s e = [x | x <- (getPrimeResults' s e), fst x == True]

-- 素数だけ取得
getPrimeNumbers :: Integer -> Integer -> [Integer]
getPrimeNumbers s e = [x | x <- map (snd) (getPrimeResults'' s e)]

-- 指定した数の次の素数を探す
nextPrime :: Integer -> Integer
nextPrime n
    | isPrime nextNum = nextNum
    | otherwise = nextPrime nextNum
    where nextNum = n+1

-- 素因数分解の 1 ステップ
-- 引数で指定された数を割り切れる一番小さな素数を探す
-- t:割られる数
-- n:割れるか試している数
-- 負の数が渡された場合、指定された数の絶対値を割り切れる一番小さな素数を返す
-- 今はまだ異常家は無視。よくわからんし。
primeFactor'''' :: Integer -> Integer -> Integer
primeFactor'''' t n
    | t == 0 = 0 -- 本当なら価なしか。とりあえず入りを返しておく
    | t == 1 = 1 -- 本当なら価なしか。とりあえず入りを返しておく
    | t < 0 = primeFactor'''' (abs t) n -- 負の数が来たら絶対値で計算
    | isPrime t = t
    | mod t n == 0 = n
    | otherwise = primeFactor'''' t (nextPrime n)

-- 素因数分解の 1 ステップ
-- 引数で指定された数を割り切れる一番小さな素数を探す
primeFactor''' :: Integer -> Integer
primeFactor''' t
    | t <= 0 = 0 -- 本当なら価なしか。とりあえず入りを返しておく
    | t == 1 = 1 -- 本当なら価なしか。とりあえず入りを返しておく
    | otherwise = primeFactor'''' t 2

-- 素因数分解の 1 ステップ
-- 指定された値を、ひとつの素数とそれ意外に分割する
primeFactor'' :: Integer -> [Integer]
primeFactor'' t
    | t <= 0 = 0:[] -- 本当なら価なしか。とりあえず入りを返しておく
    | t == 1 = 1:[] -- 本当なら価なしか。とりあえず入りを返しておく
    | isPrime t = t:[]
    | otherwise = pf:(div t pf):[]
    where pf = primeFactor''' t

-- 素因数分解の 1 ステップ
-- [素数]:[検証する数] のリストを受け取り、
-- 検証する数を割り切れる最小の素数を取得し、
-- [素数]:[検証する数を割り切れる最小の素数]:[割られ検証する数]
-- のリストにして返す。
primeFactor' :: [Integer] -> [Integer]
primeFactor' t
    | targetFactor <= 0 = 0:[] -- 本当なら価なしか。とりあえず入りを返しておく
    | targetFactor == 1 = 1:[] -- 本当なら価なしか。とりあえず入りを返しておく
    | isPrime targetFactor = t -- 最後の要素が素数であれば、リスト内の数は全て素数
    | otherwise = (init t) ++ (primeFactor' (primeFactor'' targetFactor)) -- 最後の要素を再帰的に分解していき、リストの末尾に結合
    where targetFactor = last t

-- 素因数分解する
-- 指定された数を素因数分解し、素因数のリストを返す。
-- 負の数が渡された場合、
-- [-1]:[絶対値で素因数分解したリスト]
-- というリストを返す。
-- 0, 1 の場合はそれぞれ [0], [1] を返す。
primeFactor :: Integer -> [Integer]
primeFactor t
    | t < 0 = primeFactor' ((-1):(-t):[])
    | otherwise = primeFactor' (t:[])

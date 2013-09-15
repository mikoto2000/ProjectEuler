-- フィボナッチ数列の n 番目の要素の値を取得
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

-- フィボナッチ数列のリスト作成
fibonacci :: [Integer]
fibonacci = 1:1:zipWith (+) fibonacci (tail fibonacci)

-- 「項の値が指定された数を超えない」最後のインデックス番号
-- 与えられたリストは昇順でソート済みとする
maxIndex :: [Integer] -> Integer -> Int
maxIndex list threshold =  maxIndex' list threshold 0

maxIndex' :: [Integer] -> Integer -> Int -> Int
maxIndex' list threshold index =
    if (list !! index) > threshold
        then index - 1
        else maxIndex' list threshold (index + 1)

main::IO()
main = print $ foldr (+) 0 evenFib
    where evenFib = [x | x <- tail $ take takeNumber fibonacci, even x]
          takeNumber = (maxIndex fibonacci 4000000) + 1


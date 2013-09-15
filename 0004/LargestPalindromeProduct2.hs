import Data.List

-- 3桁 x 3桁 の演算結果のリスト
target::[Integer]
target = [x * y | x <- [100..999], y <- [100..999]]

-- 回分数判定
isPalindromeNumber :: Integer -> Bool
isPalindromeNumber num = num_str_rev == num_str
    where num_str_rev =  reverse num_str
          num_str = show num

main :: IO()
main = print $ maximum palindromeNumber -- 最大値を表示
    -- 回分数のみ抽出
    where palindromeNumber = [x | x <- target, isPalindromeNumber x] 

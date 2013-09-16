-- 最初の 100 個の自然数
target :: [Integer]
target = [1..100]

sumSquare :: [Integer] -> Integer
sumSquare nums = foldr (+) 0 squares
    where squares = map (\x -> x ^ 2) nums

squareSum :: [Integer] -> Integer
squareSum nums = (foldr (+) 0 nums) ^ 2

main :: IO()
main = print $ squareSum target - sumSquare target

-- TODO: Integer の累乗で警告が出るので原因と対策を調べる

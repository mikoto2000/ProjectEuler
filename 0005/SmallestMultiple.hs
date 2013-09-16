-- 3桁 x 3桁 の演算結果のリスト
target::[Integer]
target = [1..]

-- 
isEvenlyDivisible1to20 :: Integer -> Bool
isEvenlyDivisible1to20 num = foldr (&&) True boolList
    where boolList = map (\x -> isDivisible num x) [1..20]

-- target が num で 割り切れるかを判定
-- True  : 割り切れる
-- False : 割り切れない
isDivisible :: Integer -> Integer -> Bool
isDivisible targetNum num = (mod targetNum num) == 0

main :: IO()
main = print $ take 1 $ [x | x <- [1..2432902008176640000], isEvenlyDivisible1to20 x]

multiples3and5 :: [Integer] -> Integer
multiples3and5 nums = foldr (+) 0 num35 -- リストの中身を足し合わせる
    -- 3 の倍数と 5 の倍数が格納されたリストを作成
    where num35 = [x | x <- nums, mod x 3 == 0 || mod x 5 == 0]

main::IO()
main = print $ multiples3and5 [1..999]

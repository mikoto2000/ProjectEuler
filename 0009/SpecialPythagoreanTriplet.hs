import Data.List

main :: IO()
main = print $ product $ head specialPythagoreanTriplets
    where
        specialPythagoreanTriplets = 
            -- x + y + z == 1000 なので、1 辺 998 未満のはず
            -- (本当は 998 未満だけどきりが悪いので 1000 にしてしまった)
            [[x,y,z] | x <- [1..1000], y <- [1..1000], z <- [1..1000]
            -- ピタゴラス数の条件
            , x^2 + y^2 == z^2
            -- 今回の「特別なピタゴラス数」の条件
            , x + y + z == 1000]

-- TODO: 警告の削除

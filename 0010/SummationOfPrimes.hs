import Data.List(sum)
-- need `cabal install primes'
import Data.Numbers.Primes(primes)

-- 2000000 以下の素数の合計を計算して表示
main :: IO()
main = print $ sum $ takeWhile (<=2000000) primes

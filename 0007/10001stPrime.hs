import Prime(nextPrime)

primeNumbers :: [Integer]
primeNumbers = primeNumbers' $ nextPrime 1

primeNumbers' :: Integer -> [Integer]
primeNumbers' prime = prime : primeNumbers' (nextPrime prime)

main :: IO()
main = print $ primeNumbers !! 10000

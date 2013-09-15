import Prime

main::IO()
main = print $ maximum primeFactors
    where primeFactors = primeFactor 600851475143

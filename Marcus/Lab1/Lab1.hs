import Test.QuickCheck

power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0         = 1
power n k         = n * power n (k-1)
----------------------------------------------------------------------------
-- Part 1
{-
As stated power n 0 takes one step, power n 1 takes one step and then
uses power n 0 which also takes 1 step for a total of 2 steps. Power n 2
takes 1 step then uses power n 1 which takes 2 steps and so it takes
3 steps in total. The power n k function takes k+1 steps for all k
which are positive integers.
-}
----------------------------------------------------------------------------
-- Part 2
power1 :: Integer -> Integer -> Integer
power1 n k | k < 0     = error "power: negative argument"
power1 n k |otherwise  = product(replicate (fromInteger k) n)
----------------------------------------------------------------------------
-- Part 3
power2 :: Integer -> Integer -> Integer
power2 n k  | k < 0             = error "power: negative argument"
            | k == 0            = 1
            | even k            = power2 (n*n) (k `div` 2)
            | otherwise         = n * power2 (n) (k-1)
----------------------------------------------------------------------------
-- Part 4a
{-
The functions are all defined to run for integers and have errors for
negative integers. Some intresting cases would be when n = 0, when k = 0,
at least one test for k even and one for k odd.
k even and k odd are mostly intresting for validating power2. '
Also a test for when is negative would be intresting as that is allowed.
Also a test for some large n and k.
Some testcases would be:
n = 0, k = 25
n = 0, k = 24
n = 24, k = 0
n = 25, k = 0
n = 0, k = 0
n = 24, k = 24
n = 24, k = 25
n = -25, k = 0
n = -25, k = 24
n = -25, k = 25
n = 134543, k = 18627
n = -38544, k = 38432
-}
----------------------------------------------------------------------------
-- Part 4b
{-
Check if the result of power and power1 are equal and if the result
of power1 and power2 are equal. If power and power1 are equal and
power1 and power2 are equal then power and power2 must be also be equal.
-}
prop_powers :: Integer -> Integer -> Bool
prop_powers n k   = (power n k == power1 n k) && (power1 n k == power2 n k)

----------------------------------------------------------------------------
-- Part 4c
-- List of tuples of the cases mentioned above
testlist = [(0, 24),(0, 25),(24, 0),(25, 0),(0, 0), (24, 24),(24, 25),
         ((-25), 0),((-25), 24),((-25), 25),(134543, 18627),
         ((-38544), 38432)]

-- The List comprehension returns a list of Bool values, "and" is needed
-- for the function to return a single value stating whether all tests
-- were successful or if one or more failed
run_test :: Bool
run_test = and [prop_powers x y | (x,y) <- testlist]

-- Just for a nice print than True/False
start_tests :: IO ()
start_tests | run_test      = putStrLn "All tests based"
            | otherwise     = putStrLn "Test failed"
----------------------------------------------------------------------------
-- Part 4d
{-
The problem is that quickCheck might generate a negative k, this is one
solution which just takes the absolute value of k. Another solution would
be to write another instance of generator but that requires more code.
-}
prop_powers' :: Integer -> Integer -> Bool
prop_powers' n k = prop_powers n (abs(k))

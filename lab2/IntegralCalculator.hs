import Control.Parallel.Strategies

-- Define the function f(x) = x^2
f :: Double -> Double
f x = x ** 2

-- Trapezoidal rule implementation for a small segment
trapezoidal :: (Double -> Double) -> Double -> Double -> Double
trapezoidal func a b = (func a + func b) / 2 * (b - a)

-- Parallel computation of the integral
parallelIntegral :: (Double -> Double) -> Double -> Double -> Int -> Double
parallelIntegral func a b n =
    let
        h = (b - a) / fromIntegral n
        subIntervals = [a, a + h .. b]
        subIntegrals = [trapezoidal func x (x + h) | x <- init subIntervals]
    in
        sum (parMap rdeepseq id subIntegrals)

-- Main function to calculate and print the integral
main :: IO ()
main = do
    let integral = parallelIntegral f 1 2 1000  -- Calculating the integral from 1 to 2
    putStrLn $ "The integral of f(x) from 1 to 2 is: " ++ show integral


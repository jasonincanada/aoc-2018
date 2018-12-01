{- Advent of Code 2018

   Day: 1 - Chronal Calibration
   URL: http://adventofcode.com/2018/day/1
-}

pre :: [String] -> [Int]
pre = map (read . trimPlus)
  where trimPlus ('+':xs) = xs
        trimPlus xs       = xs

process :: [Int] -> Int
process = sum

main :: IO ()
main = do
  file <- readFile "inputs/1.txt"
  let numbers = lines file
  let prep    = pre numbers
  let day1a   = process prep
  putStrLn $ show day1a

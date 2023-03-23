module Main (main) where
import Test.HUnit
import DNDTests

main :: IO Counts
main = runTestTT testSuite


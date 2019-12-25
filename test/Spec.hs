import           BoardTest
import           SolveTest
import           Test.HUnit

tests :: Test
tests = TestList [boardTests, solveTests]

main :: IO Counts
main = runTestTT tests

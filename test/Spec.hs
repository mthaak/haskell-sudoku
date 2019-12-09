import           BoardTest
import           Test.HUnit

tests :: Test
tests = boardTests

main :: IO Counts
main = runTestTT tests

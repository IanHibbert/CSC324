import Test.HUnit
import Ex6 (primes, Tree(..), treeHeight)

primesTests :: Test
primesTests = TestList [
    [2,3,5,7,11] ~=? take 5 primes,
    13 ~=? primes !! 5
    ]

treeHeightTests :: Test
treeHeightTests = TestList [
    0 ~=? treeHeight Empty,
    1 ~=? treeHeight (Node 1 Empty Empty),
    2 ~=? treeHeight (Node 1 (Node 5 Empty Empty)
                         (Node 7 Empty Empty))
    ]

main :: IO ()
main = do
    printCount "primes" primesTests
    printCount "treeHeight" treeHeightTests


-- Test runner helpers. You are not responsible for this code.
justGetCount :: Test -> IO Counts
justGetCount tests = fmap fst (runTestText (PutText (\_ _ () -> return ())  ()) tests)

printCount :: String -> Test -> IO ()
printCount name tests = do
        count <- justGetCount tests
        putStrLn (name ++ ": " ++ show count)

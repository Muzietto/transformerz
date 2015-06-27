
module TestSafePrelude where 

  import Test.HUnit
  import SafePrelude
  
  testSafeHeadForEmptyList :: Test
  testSafeHeadForEmptyList = 
      TestCase $ assertEqual "Should return Nothing for empty list"
                             Nothing (safeHead ([]::[Int]))

  testSafeHeadForNonEmptyList :: Test
  testSafeHeadForNonEmptyList =
      TestCase $ assertEqual "Should return (Just head) for non empty list" (Just 1)
                 (safeHead ([1]::[Int]))

  main :: IO Counts
  main = runTestTT $ TestList [testSafeHeadForEmptyList, testSafeHeadForNonEmptyList]
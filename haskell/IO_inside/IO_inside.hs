import System.IO.Unsafe

main = undefined

type RW = Integer

putString :: String -> RW -> ((), RW)
putString str world = (unsafePerformIO $ putStrLn str, world + 1)

getString :: RW -> (String, RW)
getString world = let input = unsafePerformIO getLine
  in (input, world + 1)

ask :: String -> RW -> (String, RW)
ask str world0 = let ((), world1) = putString str world0
                     (res, world2) = getString world1
                 in (res, world2)

personalData :: RW -> (String, RW)
personalData w0 = let (name, w1) = ask "What is your name?" w0
                      (age, w2) = ask "How old are you?" w1
                  in (name ++ " " ++ age, w2)

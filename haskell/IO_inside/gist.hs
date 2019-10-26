import Debug.Trace (trace)

-- Try to implement and use a `putString` function without the RW and
-- see the behaviour of calling it and calling it multiple times

-- type RW = RW
type RW = Integer

type MIO a = (RW -> (a, RW))

-- Try not to put `(show w)` in the trace and look at the order of the traces
putString :: String -> MIO ()
putString s w = (trace $ "putString " ++ s ++ (show w)) ((), w + 1)

-- Try not to put `(show w)` in the trace and look at the order of the traces
-- getString :: RW -> (String, RW)
getString :: MIO String
getString w = (trace $ "getString" ++ (show w)) ("Hello", w + 1)

-- entrypoint :: RW -> ((), RW)
entrypoint :: MIO ()
entrypoint w0 = let (a, w1) = ask "What is your name?" w0
                    (b, w2) = ask "How old are you?" w1
                    ask s wi0 = let ((), wi1) = (putString s) wi0
                                    (r, wi2) = getString wiuntil1
                                in (r, wi2)
                in ((), w2)

entrypoint2 :: Integer -> MIO ()
entrypoint2 n = when (n > 0) (putString "Hello2")

entrypoint3 :: Integer -> MIO ()
entrypoint3 n = untilZero n (putString "Hello3")

-- when :: Bool -> (RW -> ((), RW)) -> RW -> ((), RW)
-- when :: Bool -> MIO () -> RW -> ((), RW)
when :: Bool -> MIO () -> MIO ()
when condition action world =
  if condition
  then action world
  else ((), world)

-- untilZero :: Integer -> (RW -> ((), RW)) -> RW -> ((), RW)
-- untilZero :: Integer -> MIO () -> RW -> ((), RW)
untilZero :: Integer -> MIO () -> MIO ()
untilZero n action w0 = when (n > 0) (untilZero (n - 1) action) w1
  where (_, w1) = action w0

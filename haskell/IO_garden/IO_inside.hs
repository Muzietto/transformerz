import Debug.Trace (trace)

type RW = Integer

putString :: String -> RW -> ((), RW)
putString s w = (trace $ "putString " ++ s ++ (show w)) ((), w + 1)

getString :: RW -> (String, RW)
getString w = (trace $ "getString" ++ (show w)) ("Hello", w + 1)

entrypoint :: RW -> ((), RW)
entrypoint w0 = let (a, w1) = ask "What is your name?" w0
                    (b, w2) = ask "How old are you?" w1
                    ask s wi0 = let ((), wi1) = putString s wi0
                                    (r, wi2) = getString wi1
                                in (r, wi2)
                in ((), w2)
                

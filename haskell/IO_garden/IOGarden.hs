module IOGarden where


main = do
  a <- ask "What is your name?"
  b <- ask "How old are you?"
  return ()

ask s = do
  putStr s
  readLn

askoworld :: Read a => String -> String -> IO (IO a, String)
askoworld s world = do
  putStrLn s
  return (readLn, world ++ "x")

-- maino2 world0 = let (a, world1) = askoworld "nome?" world0
--   let (b, world2) = askoworld "cognome?" world1
--   in ((),world2)

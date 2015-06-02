module Transformers where
  import Control.Monad.Identity
  import Control.Monad.Error
  import Control.Monad.Reader
  import Control.Monad.State
  import Control.Monad.Writer
  import Data.Maybe
  import qualified Data.Map as Map
  
  type Name = String -- variable name
  -- expressions
  data Exp = Lit Integer     -- constant
           | Var Name        -- var
           | Plus Exp Exp    -- addition
           | Abs Name Exp    -- abstraction/lambda
           | App Exp Exp     -- application
           deriving (Show)
  -- values
  data Value = IntVal Integer -- int
             | FunVal Env Name Exp -- environment, argName, body
             deriving (Show)
  type Env = Map.Map Name Value -- dictionary varName:varValue

  -- expressions for exercises
  -- var xxxx, yyyy
  watIsXxxx = Var "xxxx"
  watIsYyyy = Var "yyyy"
  two_vars_env = Map.insert "xxxx" (IntVal 123) (Map.insert "yyyy" (IntVal 234) Map.empty)
  xPlusY = Plus (Var "xxxx") (Var "yyyy")
  -- \x -> x
  lambdina = Abs "x" (Var "x")
  -- 12 + (\x -> x)(4 + 2)
  exampleExp = Plus (Lit 12) (App lambdina (Plus (Lit 4) (Lit 2)))

  eval0 :: Env -> Exp -> Value
  eval0 env (Lit n)            = IntVal n
  eval0 env (Var varName)      = fromJust $ Map.lookup varName env -- env contains VALUES, not expressions!
  eval0 env (Plus exp1 exp2)   = let IntVal i1 = eval0 env exp1
                                     IntVal i2 = eval0 env exp2
                                 in IntVal (i1 + i2)
  eval0 env (Abs varName body) = FunVal env varName body
  eval0 env (App exp1 exp2)    = let val1 = eval0 env exp1
                                     val2 = eval0 env exp2
                                 in case val1 of
                                   -- val1 è un FunVal, non un Abs!!
                                   FunVal env' nn body -> eval0 (Map.insert nn val2 env') body

  -- exercises
  -- eval0 two_vars_env watIsXxxx -- IntVal 123
  -- eval0 two_vars_env xPlusY -- IntVal 357
  -- eval0 Map.empty (Plus (Lit 123) (Abs "qwe" (Lit 33))) -- *** Exception: Irrefutable pattern failed for pattern Transformers.IntVal i2
  -- eval0 two_vars_env (Var "pippo") -- *** Exception: Maybe.fromJust: Nothing
  -- eval0 Map.empty lambdina -- FunVal (fromList []) "x" (Var "x")
  -- eval0 two_vars_env lambdina -- FunVal (fromList [("xxxx",IntVal 123),("yyyy",IntVal 234)]) "x" (Var "x")
  -- eval0 Map.empty exampleExp -- IntVal 18

  -- let's get monadic
  type Eval1 = Identity
  
  runEval1 :: Eval1 a -> a
  runEval1 = runIdentity

  eval1 :: Env -> Exp -> Eval1 Value
  eval1 env (Lit n)             = return $ IntVal n
  eval1 env (Var varName)       = return $ fromJust $ Map.lookup varName env -- env contains VALUES, not expressions!
  eval1 env (Plus exp1 exp2)    = do IntVal i1 <- eval1 env exp1
                                     IntVal i2 <- eval1 env exp2
                                     return $ IntVal $ i1 + i2
  eval1 env (Abs varName body)  = return $ FunVal env varName body
                                  -- \varName' -> body'
  eval1 env (App exp1 exp2)     = do FunVal env' varName' body' <- eval1 env exp1
                                     val2 <- eval1 env exp2
                                     eval1 (Map.insert varName' val2 env') body'

  -- exercises
  -- runEval1 $ eval1 two_vars_env watIsXxxx -- IntVal 123
  -- runEval1 $ eval1 two_vars_env xPlusY -- IntVal 357
  -- runEval1 $ eval1 Map.empty (Plus (Lit 123) (Abs "qwe" (Lit 33))) *** Exception: Pattern match failure in do expression
  -- runEval1 $ eval1 two_vars_env (Var "pippo") -- *** Exception: Maybe.fromJust: Nothing
  -- runEval1 $ eval1 Map.empty exampleExp -- IntVal 18

  -- handling errors
  type Eval2 = ErrorT String Eval1

  runEval2 :: Eval2 a -> Either String a
  runEval2 = \x -> runEval1 (runErrorT x)

  eval2 :: Env -> Exp -> Eval2 Value
  eval2 env (Lit n)          = return $ IntVal n -- why not Right?
  eval2 env (Var name)       = let decoding = Map.lookup name env
                               in case decoding of
                                 Just x -> return x -- env contains VALUES, not expressions!
                                 Nothing -> throwError $ "no decoding available for " ++ name
  eval2 env (Plus exp1 exp2) = do v1 <- eval2 env exp1
                                  v2 <- eval2 env exp2
                                  case (v1, v2) of
                                    (IntVal i1, IntVal i2) -> return $ IntVal $ i1 + i2
                                    _ -> throwError "type error in sum"
  eval2 env (Abs name body)  = return $ FunVal env name body
  eval2 env (App exp1 exp2)  = do val1 <- eval2 env exp1
                                  val2 <- eval2 env exp2
                                  case val1 of
                                    FunVal env' name' body' -> eval2 (Map.insert name' val2 env') body'
                                    _ -> throwError $ "type error in application"
  
  -- exercises
  -- runEval2 $ eval2 two_vars_env watIsXxxx -- Right (IntVal 123)
  -- runEval2 $ eval2 two_vars_env xPlusY -- Right (IntVal 357)
  -- runEval2 $ eval2 Map.empty (Plus (Lit 123) (Abs "qwe" (Lit 33))) -- Left "type error in sum"
  -- runEval2 $ eval2 two_vars_env (Var "pippo") -- Left "no decoding available for pippo"
  -- runEval2 $ eval2 Map.empty lambdina -- Right (FunVal (fromList []) "x" (Var "x"))
  -- runEval2 $ eval2 Map.empty exampleExp -- Right (IntVal 18)

  -- hide environment
  type Eval3 = ReaderT Env Eval2
  
  runEval3 :: Env -> Eval3 a -> Either String a
  runEval3 env ev = runEval1 $ runErrorT $ runReaderT ev env
  
  eval3 :: Exp -> Eval3 Value
  eval3 (Lit n)            = return $ IntVal n
  eval3 (Var name)         = do env <- ask
                                case (Map.lookup name env) of
                                  Just x -> return x -- env contains VALUES, not expressions!
                                  Nothing -> throwError $ "no decoding available for " ++ name
  eval3 (Plus exp1 exp2)   = do val1 <- eval3 exp1
                                val2 <- eval3 exp2
                                case (val1, val2) of
                                  (IntVal i1, IntVal i2) -> return $ IntVal $ i1 + i2
                                  _ -> throwError "type error in sum"
  eval3 (Abs argname body) = do env <- ask
                                return $ FunVal env argname body
  eval3 (App exp1 exp2)    = do val1 <- eval3 exp1
                                val2 <- eval3 exp2
                                case val1 of
                                  FunVal env argname body -> local (const (Map.insert argname val2 env)) $ eval3 body 
                                  _ -> throwError "type error in application"
  -- exercises
  -- runEval3 two_vars_env $ eval3 watIsXxxx -- Right (IntVal 123)
  -- runEval3 two_vars_env $ eval3 xPlusY -- Right (IntVal 357)
  -- runEval3 Map.empty $ eval3 lambdina -- Right (FunVal (fromList []) "x" (Var "x"))
  -- runEval3 two_vars_env $ eval3 (Var "pippo") -- Left "no decoding available for pippo"
  -- runEval3 Map.empty $ eval3 exampleExp -- Right (IntVal 18)

  -- adding state to count recursive invocations
  type Eval4 = ReaderT Env (ErrorT String (StateT Integer Eval1))
  
  runEval4 :: Env -> Integer -> Eval4 a -> (Either String a, Integer)
  runEval4 env s exp = runEval1 $ runStateT (runErrorT (runReaderT exp env)) s

  tick = do n <- get
            put (n + 1)

  eval4 :: Exp -> Eval4 Value
  eval4 (Lit n)            = do tick
                                return $ IntVal n
  eval4 (Var name)         = do tick
                                env <- ask
                                case (Map.lookup name env) of
                                  Just x -> return x -- env contains VALUES, not expressions!
                                  Nothing -> throwError $ "no decoding for " ++ name
  eval4 (Plus exp1 exp2)   = do tick
                                val1 <- eval4 exp1
                                val2 <- eval4 exp2
                                case (val1, val2) of
                                  (IntVal i1,IntVal i2) -> return $ IntVal $ i1 + i2
                                  _ -> throwError $ "type error in sum"
  eval4 (Abs argname body) = do tick
                                env <- ask
                                return $ FunVal env argname body
  eval4 (App exp1 exp2)    = do tick
                                val1 <- eval4 exp1
                                val2 <- eval4 exp2
                                case val1 of
                                  FunVal env argname body -> local (const (Map.insert argname val2 env)) $ eval4 body 
                                  _ -> throwError "type error in application"
                                
  -- exercises
  -- runEval4 Map.empty 0 $ eval4 $ Lit 12 -- (Right (IntVal 12),1)
  -- runEval4 two_vars_env 0 $ eval4 watIsXxxx -- (Right (IntVal 123),1)
  -- runEval4 two_vars_env 0 $ eval4 $ Var "pippo" -- (Left "no decoding for pippo",1)
  -- runEval4 two_vars_env 0 $ eval4 xPlusY -- (Right (IntVal 357),3)
  -- runEval4 Map.empty 0 $ eval4 lambdina -- (Right (FunVal (fromList []) "x" (Var "x")),1)
  -- runEval4 Map.empty 0 $ eval4 exampleExp -- (Right (IntVal 18),8)

  -- adding logging to do useful stuff
  type Eval5 = ReaderT Env (ErrorT String (WriterT [String] (StateT Integer Eval1)))

  runEval5 :: Env -> Integer -> Eval5 a -> ((Either String a,[String]), Integer)
  runEval5 env s exp = runEval1 $ runStateT (runWriterT (runErrorT (runReaderT exp env))) s
  
  eval5 :: Exp -> Eval5 Value
  eval5 (Lit n) = do tick
                     return $ IntVal n
                      
  -- exercises
  
  
  
  
  
  
  
  
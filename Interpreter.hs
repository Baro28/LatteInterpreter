module Interpreter where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import AbsGramatyka

data Value =
  IntVal Integer
  | BoolVal Bool
  | StringVal String
  | FunVal FunType [Arg] Block
  | Null
  deriving (Show, Eq)

data ReturnValue = 
  RetVal Expr
  | None
  | BrkVal
  | ConVal
  deriving (Show, Eq)

type Loc = Int

type Store = Map.Map Loc Value
type Env = Map.Map Ident Loc

type Interpreter = ReaderT Env (StateT Store (ExceptT String IO))

evalBlock :: Block -> Interpreter (Env, ReturnValue)
evalBlock (Block pos (stmt:stmts)) = do
  (env, retVal) <- evalStmt stmt
  if retVal == None then
    local (const env) (evalBlock (Block pos stmts))
  else
    return (env, retVal)
evalBlock (Block pos []) = returnNone

interpret prog = runExceptT $ runStateT (runReaderT (evalProg prog) Map.empty) Map.empty

parseArg :: (Arg, PassedArg) -> Interpreter Env
parseArg argPair = do
  env <- ask
  case argPair of
    ((Arg _ (Ref _ t) funName), (ERef _ passedName)) -> do
      case Map.lookup passedName env of
        Just loc -> return (Map.insert funName loc env)
        Nothing -> throwError "passed variable is undefined"
    ((Arg _ (Value pos t) funName), (EVal _ e)) -> do
      (declareVar t (Init pos funName e))
    (_, _) -> throwError "wrong argument type"

parseArgs :: [(Arg, PassedArg)] -> Interpreter Env
parseArgs [] = do
  env <- ask
  return env
parseArgs (argPair:argPairs) = do
  env <- parseArg argPair
  local (const env) (parseArgs argPairs)

computeRetVal :: ReturnValue -> Interpreter Value
computeRetVal retVal = do
  case retVal of
    (RetVal e) -> (evalExpr e)
    (None) -> return Null
    (BrkVal) -> throwError "break outside of loop"
    (ConVal) -> throwError "continue outside of loop"

startFunction :: [Arg] -> [PassedArg] -> Block -> Interpreter Value
startFunction args passedArgs block = do
  when (length args /= length passedArgs)
    (throwError "wrong number of passed arguments")
  
  env <- ask
  funEnv <- local (const env) (parseArgs (zip args passedArgs))
  (newEnv, retVal) <- local (const funEnv) (evalBlock block)
  val <- local (const newEnv) (computeRetVal retVal)
  return val


evalExpr :: Expr -> Interpreter Value
evalExpr (EVar pos i) = do
  store <- get
  env <- ask
  case Map.lookup i env of
    Just loc -> case Map.lookup loc store of
      Just val -> return val
      Nothing -> throwError "variable without value"
    Nothing -> throwError "undefined variable"
evalExpr (ELitInt pos a) = return (IntVal a)
evalExpr (ELitTrue pos) = return (BoolVal True)
evalExpr (ELitFalse pos) = return (BoolVal False)
evalExpr (EApp pos i passedArgs) = do
  store <- get
  env <- ask
  case Map.lookup i env of
    Just loc -> case Map.lookup loc store of
      Just (FunVal t args block) -> (startFunction args passedArgs block)
      Nothing -> throwError "function without value"
    Nothing -> throwError "undefined function name"
evalExpr (EString pos a) = return (StringVal a)
evalExpr (Neg pos a) = do
  v <- evalExpr a
  case v of
    (IntVal n) -> return (IntVal (-n))
    _ -> throwError "negate applied to non int value"
evalExpr (Not pos a) = do
  v <- evalExpr a
  case v of
    (BoolVal n) -> return (BoolVal (not n))
    _ -> throwError "not applied to non bool value"
evalExpr (EMul pos a op b) = do
  v1 <- evalExpr a
  v2 <- evalExpr b
  case (v1, v2) of
    (IntVal x, IntVal y) -> case op of
      (Times pos) -> return (IntVal (x * y))
      (Div pos) -> case y of
        0 -> throwError "can not divide by zero"
        _ -> return (IntVal (x `div` y))
      (Mod pos) -> case y of
        0 -> throwError "can not modulo by zero"
        _ -> return (IntVal (x `mod` y))
    _ -> throwError "multiplication is only applicable to integers"
evalExpr (EAdd pos a op b) = do
  v1 <- evalExpr a
  v2 <- evalExpr b
  case (v1, v2) of
    (IntVal x, IntVal y) -> case op of
      (Plus pos) -> return (IntVal (x + y))
      (Minus pos) -> return (IntVal (x - y))
    _ -> throwError "addition is only applicable to integers"
evalExpr (ERel pos a op b) = do
  v1 <- evalExpr a
  v2 <- evalExpr b
  case (v1, v2) of
    (IntVal x, IntVal y) -> return (cmp x op y)
    (BoolVal x, BoolVal y) -> return (cmp x op y)
    _ -> throwError "relation is only applicable to same types of integers and booleans"
evalExpr (EAnd pos a b) = do
  v1 <- evalExpr a
  v2 <- evalExpr b
  case (v1, v2) of
    (BoolVal x, BoolVal y) -> return (BoolVal (x && y))
    _ -> throwError "and is only applicable to booleans"
evalExpr (EOr pos a b) = do
  v1 <- evalExpr a
  v2 <- evalExpr b
  case (v1, v2) of
    (BoolVal x, BoolVal y) -> return (BoolVal (x || y))
    _ -> throwError "or is only applicable to booleans"

returnNone :: Interpreter (Env, ReturnValue)
returnNone = do
  env <- ask
  return (env, None)

getCondValue :: Value -> Interpreter Bool
getCondValue val = do
  case val of
    (BoolVal val) -> return val
    _ -> throwError "condition is not a boolean value"

evalStmt :: Stmt -> Interpreter (Env, ReturnValue)
evalStmt (Empty pos) = do
  returnNone
evalStmt (BStmt pos block) = do
  evalBlock block
evalStmt (Decl pos t []) = do
  returnNone
evalStmt (Decl pos t (decl:decls)) = do
  env <- declareVar t decl
  local (const env) (evalStmt (Decl pos t decls))
evalStmt (Ass pos ident e) = do
  val <- evalExpr e
  env <- ask
  case Map.lookup ident env of
    Just loc -> modify (Map.insert loc val) >> returnNone
    Nothing -> throwError "undefined variable assignment"
evalStmt (Incr pos ident) = do
  store <- get
  env <- ask
  case Map.lookup ident env of
    Just loc -> case Map.lookup loc store of
      Just val -> case val of
        (IntVal val) -> modify (Map.insert loc (IntVal (val + 1)))
        _ -> throwError "incrementing of non integer variable"
      Nothing -> throwError "incrementing variable without value"
    Nothing -> throwError "undefined variable incrementing"
  returnNone
evalStmt (Decr pos ident) = do
  store <- get
  env <- ask
  case Map.lookup ident env of
    Just loc -> case Map.lookup loc store of
      Just val -> case val of
        (IntVal val) -> modify (Map.insert loc (IntVal (val - 1)))
        _ -> throwError "decrementing of non integer variable"
      Nothing -> throwError "decrementing variable without value"
    Nothing -> throwError "undefined variable decrementing"
  returnNone
evalStmt (Ret pos e) = do
  env <- ask
  return (env, RetVal e)
evalStmt (VRet pos) = do
  returnNone
evalStmt (Cond pos e block) = do
  val <- evalExpr e
  cond <- getCondValue val
  if cond then
    evalBlock block
  else
    returnNone
evalStmt (CondElse pos e block1 block2) = do
  BoolVal cond <- evalExpr e
  if cond then
    evalBlock block1
  else
    evalBlock block2
evalStmt (While pos e block) = do
  BoolVal cond <- evalExpr e
  if cond then
    do
      retVal <- evalBlock block
      case retVal of
        (_, None) -> evalStmt (While pos e block)
        (_, RetVal e) -> do
          env <- ask
          return (env, RetVal e)
  else
    returnNone
evalStmt (SExp pos e) = do
  val <- evalExpr e
  returnNone
evalStmt (Cont pos) = do
  env <- ask
  return (env, ConVal)
evalStmt (Break pos) = do
  env <- ask
  return (env, BrkVal)
evalStmt (Print pos e) = do
  val <- evalExpr e
  liftIO $ putStrLn $ show val
  returnNone

evalProg :: Program -> Interpreter ReturnValue
evalProg (Program pos prog) = evalTopFunctions prog

evalTopFunctions :: [TopDef] -> Interpreter ReturnValue
evalTopFunctions [] = return None
evalTopFunctions ((def@(FnDef pos t ident args block)):defs) = do
  env <- declFun t ident args block
  case ident of
    Ident "main" -> do
      evalBlock block
      return None
    _ -> local (const env) (evalTopFunctions defs)


declFun :: FunType -> Ident -> [Arg] -> Block -> Interpreter Env
declFun t ident args block = do
  env <- ask
  store <- get
  let newloc = (Map.size store) + 1
  modify (Map.insert newloc (FunVal t args block))
  return (Map.insert ident newloc env)

declareVar :: Type -> Item -> Interpreter Env
declareVar t item = do
  env <- ask
  store <- get
  let 
    newloc = (Map.size store) + 1
    name = case item of
      NoInit _ ident -> ident
      Init _ ident _ -> ident
  newVal <- case item of
    NoInit _ _ -> defaultValue t
    Init _ _ e -> evalExpr e
  modify (Map.insert newloc newVal)
  return (Map.insert name newloc env)


defaultValue :: Type -> Interpreter Value
defaultValue t = case t of
  Int _ -> return (IntVal 0)
  Str _ -> return (StringVal "")
  Bool _ -> return (BoolVal False)

cmp :: Ord a => a -> RelOp -> a -> Value
cmp a op b = case op of
  (LTH pos) -> (BoolVal (a < b))
  (LE pos) -> (BoolVal (a <= b))
  (GTH pos) -> (BoolVal (a > b))
  (GE pos) -> (BoolVal (a >= b))
  (EQU pos) -> (BoolVal (a == b))
  (NE pos) -> (BoolVal (a /= b))
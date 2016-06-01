{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module QL.Interpreter (exec,execExpr) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.State
import           Prelude                hiding (EQ, GT, lookup)
import           QL.Environment
import           QL.Language.Syntax.Ast as A
import           QL.Money
import           QL.Value.Value

type InterpreterError = String

newtype Interpreter a =
          Interp
            { runI :: StateT (Environment Value) (ExceptT InterpreterError Identity) a }
  deriving (MonadError InterpreterError, Applicative, Functor, Monad, MonadState (Environment Value))

runInterp :: Environment Value -> Interpreter a -> Either InterpreterError a
runInterp env ev = runIdentity(runExceptT (evalStateT  (runI ev) env))

execExpr :: Environment Value -> Expr -> Value
execExpr env expr =
  case runInterp env (eval expr) of
    Left _         -> Undefined
    Right val  -> val

eval :: Expr -> Interpreter Value
eval (Lit (ILit lit)) = return (IntValue lit)
eval (Lit (BLit lit)) = return (BoolValue lit)
eval (Lit (SLit lit)) = return (StringValue lit)
eval (Lit (MLit lit)) = return (MoneyValue lit)
eval (Var name) =  do
  r <- Control.Monad.State.gets (lookup name)
  case r of
    Just v  -> return v
    Nothing -> throwError ("Unbound variable '" ++ show name ++ "'")
eval (BinOp op lhs rhs) = do
  left <- eval lhs
  right <- eval rhs
  evalBinOp op left right
eval (UnOp Not rhs) = do
  right <- eval rhs
  case neg right of
    Nothing -> throwError "Cannot negate non-boolean variable"
    Just x -> return x

evalBinOp :: BinOp -> Value -> Value -> Interpreter Value
-- Undefined
evalBinOp _ Undefined _                           = return Undefined
evalBinOp _ _ Undefined                           = return Undefined
-- Ints
evalBinOp Add (IntValue x) (IntValue y)           = return (IntValue $ x + y)
evalBinOp Sub (IntValue x) (IntValue y)           = return (IntValue $ x - y)
evalBinOp Div (IntValue _) (IntValue 0)           = return Undefined -- Divide by Zero
evalBinOp Div (IntValue x) (IntValue y)           = return (IntValue $ x `div` y)
evalBinOp Mul (IntValue x) (IntValue y)           = return (IntValue $ x * y)
evalBinOp GT (IntValue x) (IntValue y)            = return (BoolValue $ x > y)
--Money + Ints
evalBinOp Add (IntValue x) (MoneyValue y)         = return (MoneyValue $ integerToMoney x + y)
evalBinOp Add (MoneyValue x) (IntValue y)         = return (MoneyValue $ x + integerToMoney y)
evalBinOp Sub (IntValue x) (MoneyValue y)         = return (MoneyValue $ integerToMoney x - y)
evalBinOp Sub (MoneyValue x) (IntValue y)         = return (MoneyValue $ x - integerToMoney y)
evalBinOp Mul (IntValue x) (MoneyValue y)         = return (MoneyValue $ integerToMoney x * y)
evalBinOp Mul (MoneyValue x) (IntValue y)         = return (MoneyValue $ x * integerToMoney y)
evalBinOp GT (IntValue x) (MoneyValue y)          = return (BoolValue $ integerToMoney x > y)
evalBinOp GT (MoneyValue x) (IntValue y)          = return (BoolValue $ x > integerToMoney y)
--Money
evalBinOp Add (MoneyValue x) (MoneyValue y)       = return (MoneyValue $ x + y)
evalBinOp Sub (MoneyValue x) (MoneyValue y)       = return (MoneyValue $ x - y)
--String
evalBinOp SConcat (StringValue x) (StringValue y) = return (StringValue $ x ++ y)
--Bool
evalBinOp And (BoolValue x) (BoolValue y)         = return (BoolValue $ x && y)
evalBinOp Or (BoolValue x) (BoolValue y)          = return (BoolValue $ x || y)
--EQ
evalBinOp EQ (StringValue x) (StringValue y)      = return (BoolValue $ x == y)
evalBinOp EQ (IntValue x) (IntValue y)            = return (BoolValue $ x == y)
evalBinOp EQ (MoneyValue x) (MoneyValue y)        = return (BoolValue $ x == y)
evalBinOp EQ (BoolValue x) (BoolValue y)          = return (BoolValue $ x == y)
-- Not supported Op
evalBinOp Add _ _                                 = throwError "Not supported"
evalBinOp Sub _ _                                 = throwError "Not supported"
evalBinOp Div _ _                                 = throwError "Not supported"
evalBinOp Mul _ _                                 = throwError "Not supported"
evalBinOp GT _ _                                  = throwError "Not supported"
evalBinOp And _ _                                 = throwError "Not supported"
evalBinOp Or _ _                                  = throwError "Not supported"
evalBinOp EQ _ _                                  = throwError "Not supported"
evalBinOp SConcat _ _                             = throwError "Not supported"

exec :: Form -> Environment Value -> Either InterpreterError (Environment Value)
exec ast r =
  case runInterp emptyEnv (execForm ast) of
    Left msg      -> Left msg
    Right a -> Right a
  where
    declare' i val env = declare env i val
    execForm (Form _ block) = do
      mapM_ ex' block
      Control.Monad.State.get
    ex' (If cond block) = do
      c <- eval cond
      when (c == BoolValue True) $ mapM_ ex' block
      return ()
    ex' (Field (CalcField info expr)) = do
      res <- eval expr
      modify (declare' (A.id info) res)
      return ()
    ex' (Field (SimpField info)) = do
      let name = A.id info
          t = A.fieldType info
      case  lookup name r of
        Just v  -> modify (declare' name v )
        Nothing -> modify (declare' name (defaultVal t))

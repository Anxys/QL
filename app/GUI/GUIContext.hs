module GUIContext(initializeGUIContext) where

import           Control.Monad
import           Control.Exception.Base
import           Data.Maybe
import           Graphics.UI.WX
import           GUIElement
import           GUIError
import           Prelude                hiding (elem)
import qualified QL.Environment         as E
import           QL.Identifier
import           QL.Interpreter
import           QL.Language.Syntax.Ast as A
import           QL.Value.Value

data GUIContext =
       GUIContext
         { appFrame    :: Frame ()
         , form        :: A.Form
         , guiElements :: [GUIQuestion]
         , environment :: Var (E.Environment Value)
         }

initializeGUIContext :: Frame () -> A.Form -> IO ()
initializeGUIContext f astForm = 
  case exec astForm E.emptyEnv of
    Left err  -> showFatalErrorDialog (InterpreterError err)
    Right initialEnv -> do  
      envRef <- varCreate initialEnv
      elems <- mapM (uncurry (initializeQuestion (ctx [] envRef))) questionFields
      context <- createContext elems envRef
      mapM_ (addHandler context)  elems
      updateGUI context
      return ()
  where
    questionFields = A.fieldConditionalDependencies astForm
    createContext elems env = return (ctx elems env)
    ctx = GUIContext f astForm

setValueFromEnv :: E.Environment Value -> GUIQuestion -> IO ()
setValueFromEnv env q = setValue q (getValue info env)
  where info = questionElemInfo q

getNewEnv :: A.Form -> E.Environment Value -> E.Environment Value
getNewEnv astForm env =
  case exec astForm env of
    Left x  -> assert False (error x)
    Right r -> r

getValue :: ElemInfo -> E.Environment Value -> Value
getValue e env = fromMaybe (defaultVal (valueType e)) (E.lookup (identifier e) env)

setFieldsVisibility :: GUIContext -> IO ()
setFieldsVisibility context = do
  env <- varGet (environment context)
  _ <- mapM_ (setVis env) (guiElements context)
  _ <- addToLayout (appFrame context) (guiElements context)
  return ()
  where
    isVisible env elem = evalConditions env (conditions (questionElemInfo elem))
    evalConditions env conds = andValues (map (execExpr env) conds)
    setVis env e = setVisibility e (isVisible env  e)

computeCalculatedFieldValues :: GUIContext -> IO ()
computeCalculatedFieldValues context = do
   env <- varGet (environment context)
   _ <- mapM_ (setValueFromEnv env) ( filter isComputed  (guiElements context))
   return ()

updateGUI :: GUIContext -> IO ()
updateGUI context = do
    computeCalculatedFieldValues context
    setFieldsVisibility context
    return ()

initializeQuestion :: GUIContext -> A.Field -> [A.Expr] -> IO GUIQuestion
initializeQuestion ctx field deps = do 
  val <- getVal (fieldInfo field)
  mkQuestion (appFrame ctx) val field deps 
    where env = varGet (environment ctx)
          getVal i = getValueFromEnv i <$> env
          getValueFromEnv e envir = fromMaybe (defaultVal (fieldType e)) (E.lookup (A.id e) envir)

updateGUIContext :: GUIContext -> Identifier -> Value -> IO ()
updateGUIContext ctx fieldIdentifier newValue = do
  oldEnv <- varGet envRef
  varSet envRef (getNewEnv astForm (E.declare oldEnv fieldIdentifier newValue))
  return ()
    where astForm = form ctx
          envRef = environment ctx

isComputed :: GUIQuestion -> Bool
isComputed = not.isSimpleQuestion

addHandler :: GUIContext -> GUIQuestion -> IO ()
addHandler ctx question
  | isCheckbox question = setHandler' (checkboxHandler ctx question)
  | isText question = setHandler' (textHandler ctx question)
  | otherwise  = assert False (error "Not text or checkbox")
    where setHandler' = setHandler question

genericHandler :: GUIContext -> GUIQuestion -> (UserInputError -> IO ()) -> IO ()
genericHandler ctx elem errorHandler = do
  result <- getElementValue elem
  case result of
    Left e -> errorHandler e
    Right newValue -> handleNewValue ctx (isComputed elem)  (identifier (questionElemInfo elem)) newValue

checkboxHandler :: GUIContext -> GUIQuestion -> IO ()
checkboxHandler ctx elem =
  genericHandler ctx elem (\e    -- This should not happen with checkboxes
                            -> assert False (error ("Input validation failed for a checkbox" ++ show e)))

textHandler :: GUIContext -> GUIQuestion -> IO ()
textHandler ctx elem =
  genericHandler ctx elem
    (\err -> unless (isComputed elem) $ do
       env <- varGet (environment ctx)
       setValueFromEnv env elem
       showUserInputErrorDialog (appFrame ctx) err
       return ())

handleNewValue :: GUIContext -> Bool -> Identifier -> Value -> IO ()
handleNewValue ctx isReadOnly fieldIdentifier newValue = do
  unless isReadOnly $ updateGUIContext ctx fieldIdentifier newValue
  updateGUI ctx
  return ()

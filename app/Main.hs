module Main where

import Control.Monad as C
import Graphics.UI.WX
import GUIError
import GUIContext
import QL.Language.Syntax.Ast as A
import QL.Language.Parser
import System.Environment

-- For some reason wx tries to consumes the args supplied at the command line so we have to clear them first
loadUI :: IO a -> IO ()
loadUI = withArgs []. start

gui :: Show a => [a] -> A.Form -> IO ()
gui warnings astForm = do
  f <- frame [text:= "Questionnaire: " ++ formName astForm, visible:= True]
  unless (null warnings) $ showWarningDialog f (Warning warnings)
  _ <- initializeGUIContext f astForm
  return ()
    where formName (A.Form name _) = show name

main :: IO ()
main = do
  arg <- getArgs
  C.when (length arg /= 1) $ loadUI (showFatalErrorDialog ArgumentError)
  result <- parseFile (getFileName arg)
  case result of
    Left e -> loadUI (showFatalErrorDialog (LoadingError e))
    Right (warnings, ast) -> loadUI $ gui warnings ast
  return ()
    where getFileName = head


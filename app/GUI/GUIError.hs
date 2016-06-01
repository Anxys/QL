module GUIError (UserInputError(..), FatalError(..),Warning(..), showWarningDialog, showFatalErrorDialog, showUserInputErrorDialog) where 

import           Graphics.UI.WX
import           System.Exit
import           System.IO
import           QL.Language.Parser (Error)
import           QL.Language.Syntax.Ast (FieldType)

data UserInputError = UserInputError FieldType

data FatalError = ArgumentError
                | LoadingError Error
                | InterpreterError String 
                deriving Show

data Warning a = Warning a

instance Show UserInputError where
  show = userInputErrorMessage

userInputErrorTitle :: UserInputError -> String
userInputErrorTitle (UserInputError _) = "Input Error"

userInputErrorMessage :: UserInputError -> String
userInputErrorMessage (UserInputError x) = "Invalid value. Expected " ++ show x

warningMessage :: Show a => Warning a -> String
warningMessage (Warning x) = show x

warningTitle :: Show a => Warning a -> String
warningTitle (Warning _) = "Warning"

fatalErrorTitle :: FatalError -> String
fatalErrorTitle ArgumentError = "Invalid Argument Error"
fatalErrorTitle (LoadingError _) = "File Loading Error"
fatalErrorTitle (InterpreterError _ ) = "Interpreter Error"

fatalErrorMessage :: FatalError -> String
fatalErrorMessage ArgumentError = "Incorrect number of arguments. Must provide a file name."
fatalErrorMessage (LoadingError e) = show e
fatalErrorMessage (InterpreterError e) = show e

showStdMessage :: Show a => a -> IO ()
showStdMessage = hPrint stderr

showDialog :: (Frame () -> String -> String -> IO ()) -> Frame () -> String -> String -> IO ()
showDialog func f title msg = do
  showStdMessage title
  showStdMessage msg
  func f title msg
  return ()

showErrorDialog :: Frame () -> Bool -> String -> String -> IO ()
showErrorDialog f exitFlag title msg = do
  showDialog errorDialog f title msg
  when exitFlag exitFailure
  return ()

showFatalErrorDialog :: FatalError -> IO ()
showFatalErrorDialog err = do
  f <- frame [visible := False]
  showErrorDialog f True title msg

  where
    title = fatalErrorTitle err
    msg = fatalErrorMessage err

showUserInputErrorDialog :: Frame () -> UserInputError -> IO ()
showUserInputErrorDialog f err = showErrorDialog f False title msg
  where
    title = userInputErrorTitle err
    msg = userInputErrorMessage err

showWarningDialog :: Show a => Frame () -> Warning a -> IO ()
showWarningDialog f warning = showDialog warningDialog f title msg
  where
    title = warningTitle warning
    msg = warningMessage warning

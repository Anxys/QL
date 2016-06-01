module GUIElement (
    GUIQuestion,
    ElemInfo,
    getElementValue,
    setValue,
    valueType,
    identifier,
    addToLayout,
    conditions,
    questionElemInfo,
    setVisibility,
    mkQuestion,
    isSimpleQuestion,
    isCheckbox,
    isText,
    setHandler,
    ) where

import           Control.Exception.Base
import           Graphics.UI.WX
import           Graphics.UI.WXCore     (rgb)
import           GUIError
import           QL.Identifier
import           QL.Language.Syntax.Ast as Ast
import           QL.Value.Value

defaultFont :: FontStyle
defaultFont = fontFixed

disabledColor :: Color
disabledColor = rgb 128 128 128

enabledColor :: Color
enabledColor = rgb 0 0 0

data ElemInfo =
       ElemInfo
         { identifier :: Identifier
         , valueType  :: FieldType
         , conditions :: [Expr]
         }

data GUIElement = Text ElemInfo (StaticText ()) (TextCtrl ())
                | Checkbox ElemInfo (StaticText ()) (CheckBox ())

data GUIQuestion = Question GUIElement
                 | CalculatedQuestion GUIElement

guiElement :: GUIQuestion -> GUIElement
guiElement (Question x) = x
guiElement (CalculatedQuestion x) = x

createElemInfo :: FieldInfo -> [Expr] -> ElemInfo
createElemInfo x = ElemInfo (Ast.id x) (Ast.fieldType x )

addToLayout :: Frame a -> [GUIQuestion] -> IO ()
addToLayout f ls = do
  set f [layout := grid 10 10 (foldr ((\x m -> elements x : m).guiElement) [] ls)]
  return ()

  where
    elements (Text _ t control) = [widget t, widget control]
    elements (Checkbox _ t control) = [widget t, widget control]


elemInfo :: GUIElement -> ElemInfo
elemInfo (Text info _ _) = info
elemInfo (Checkbox info _ _) = info

getElementValue :: GUIQuestion -> IO (Either UserInputError Value)
getElementValue = getElementValue' . guiElement

getElementValue' :: GUIElement -> IO (Either UserInputError Value)
getElementValue' (Text info _ control) = do
  val <- get control text
  if validate vType (fromDisplay' val)
    then return (Right (fromDisplay' val))
    else return (Left (UserInputError vType))

  where
    fromDisplay' = fromDisplay (valueType info)
    vType = valueType info
getElementValue' (Checkbox _ _ control) = do
  val <- get control checked
  return (Right (BoolValue val))

validate :: FieldType -> Value -> Bool
validate x = haveSameValueType (defaultVal x)

setVisibility' :: (Visible a, Visible b) => a -> b -> Bool -> IO ()
setVisibility' c1 c2 b = do
  _ <- set c1 [visible := b]
  _ <- set c2 [visible := b]
  return ()

setValue :: GUIQuestion -> Value -> IO ()
setValue question val =
  case guiElement question of
    (Text _ _ control) -> set control [text := toDisplay val]
    (Checkbox _ _ control) -> set control [checked := extractBoolValue val]


questionElemInfo :: GUIQuestion -> ElemInfo
questionElemInfo = elemInfo . guiElement

isSimpleQuestion :: GUIQuestion -> Bool
isSimpleQuestion x =
  case x of
    Question{}           -> True
    CalculatedQuestion{} -> False

mkQuestion :: Frame () -> Value -> Ast.Field -> [Expr] -> IO GUIQuestion
mkQuestion f val field deps =
  case Ast.fieldType info of
    Money   -> questionType <$> standardControl
    Integer -> questionType <$> standardControl
    String  -> questionType <$> standardControl
    Boolean -> questionType <$> createControl Checkbox (createCheckBox isComputedQuestion)
  where
    eInfo = createElemInfo info deps
    info = Ast.fieldInfo field
    questionType = if isComputedQuestion
                     then CalculatedQuestion
                     else Question
    standardControl = createControl Text (createTextControl isComputedQuestion)
    isComputedQuestion = isCalcField field
    createControl constr controlConstr = do
      qLabel <- createStaticText f (Ast.label info)
      control <- controlConstr f val
      return (constr eInfo qLabel control)

createStaticText :: Frame a -> String -> IO (StaticText ())
createStaticText f x = staticText f [text := x]

ctrlColor :: Bool -> Color
ctrlColor x = if x then disabledColor else enabledColor

createTextControl :: Bool -> Frame a -> Value -> IO (TextCtrl ())
createTextControl b f v = textEntry f [font := defaultFont, color:= ctrlColor b, text := toDisplay v]

createCheckBox :: Bool -> Frame a -> Value -> IO (CheckBox ())
createCheckBox b f (BoolValue v) = checkBox f [color:= ctrlColor b, checked := v]
createCheckBox _ _ _ = assert False (error "None boolean value supplied")

isText :: GUIQuestion -> Bool
isText question =
  case guiElement question of
    Checkbox{} -> False
    Text{}     -> True

isCheckbox :: GUIQuestion -> Bool
isCheckbox = not . isText

setHandler :: GUIQuestion -> IO () -> IO ()
setHandler question handler =
  case guiElement question of
    (Checkbox _ _ cBox)  -> set cBox [on command := handler]
    (Text _ _ textField) -> set textField [on leave := const handler]

setVisibility :: GUIQuestion -> Bool -> IO ()
setVisibility question b =
  case guiElement question of
    (Text _ s c)     -> setVisibility' c s b
    (Checkbox _ s c) -> setVisibility' c s b

module QL.Language.Syntax.Annotated.Parsing (
    QL.Language.Syntax.Annotated.Parsing.parse,
    form,
    expr,
    ifStmnt,
    ifElseStmnt,
    conditional,
    simpleField,
    calculatedField,
    ParseError,
    ) where

import           Control.Monad
import           QL.Identifier
import           QL.Language.Syntax.Annotated.AnnotatedAst     as A
import           QL.Language.Syntax.Annotated.Parsing.Extended
import           QL.Location
import           QL.Money
import           Text.Parsec.Prim                              as S
import           Text.ParserCombinators.Parsec                 as P
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token           as Token

languageDef :: LanguageDef st
languageDef =
  emptyDef
    { Token.commentStart = "/*"
    , Token.commentEnd = "*/"
    , Token.commentLine = "//"
    , Token.identStart = letter <|> char '_'
    , Token.identLetter = alphaNum
    , Token.reservedNames = [ "if"
                            , "else"
                            , "form"
                            , "true"
                            , "false"
                            , "not"
                            , "and"
                            , "or"
                            , "money"
                            , "integer"
                            , "string"
                            , "boolean"
                            ]
    , Token.reservedOpNames = [ "+"
                              , "-"
                              , "*"
                              , "/"
                              , "and"
                              , "or"
                              , "not"
                              , ":"
                              , "++"
                              , ">"
                              , "<"
                              , "=="
                              , "!="
                              , ">="
                              , "<="
                              ]
    , Token.caseSensitive = True
    }

lexer :: Token.TokenParser st
lexer = Token.makeTokenParser languageDef

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parsec String u ()
reserved = Token.reserved lexer

reservedOp :: String -> Parsec String u ()
reservedOp = Token.reservedOp lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

integer :: Parser Integer
integer = Token.integer lexer

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer

moneyT :: Parser (FieldType Location)
moneyT = addLoc0 A.Money (reserved "money") <?> "money type"

integerT :: Parser (FieldType Location)
integerT = addLoc0 Integer (reserved "integer") <?> "integer type"

boolT :: Parser (FieldType Location)
boolT = addLoc0  Boolean (reserved "boolean") <?> "bool type"

stringT :: Parser (FieldType Location)
stringT = addLoc0  String (reserved "string") <?> "string type"

fieldT :: Parser (FieldType Location)
fieldT = moneyT <|> integerT <|> stringT <|> boolT <?> "field type"

field :: Parser (Statement Location)
field = addLoc1 Field (P.try calculatedField <|> simpleField)

fieldInfo :: Parser (FieldInformation Location)
fieldInfo = do
  text <- stringLiteral
  name <- identifier
  reserved ":"
  t <- fieldT
  return (FieldInformation text (mkIdentifier name) t)

simpleField :: Parser (Field Location)
simpleField = addLoc1 SimpleField fieldInfo <?> "simple field"

calculatedField :: Parser (Field Location)
calculatedField = addLoc2 CalculatedField  (fieldInfo <* reserved "=") expr <?> "calculated field"

block :: Parser (Block Location)
block = braces (many stmnt) <?> "block"

expr :: Parser (Expression Location)
expr = buildExpressionParser opTable term <?> "expression"

-- Helper functions to make the opTable more readable
binop_ :: Location -> (Location -> BinaryOperation Location) -> Expression Location -> Expression Location -> Expression Location
binop_ l op = BinaryOperation l (op l)

-- Helper functions to make the opTable more readable
uop_ :: Location -> (Location -> UnaryOperation Location) -> Expression Location -> Expression Location
uop_ l op = UnaryOperation l (op l)

opTable :: [[Operator Char () (Expression Location)]]
opTable = [
          [ unary "not" A.Not ]
        , [ binary ">=" A.GreaterThanOrEquals AssocLeft
          , binary "<=" A.LesserThanOrEquals AssocLeft
          , binary ">" A.GreaterThan AssocLeft
          , binary "<" A.LesserThan AssocLeft
          ]
        , [ binary "==" A.Equals AssocLeft
          , binary "!=" A.NotEquals AssocLeft
          ]
        , [ binary "and" A.And AssocLeft
          , binary "or" A.Or AssocLeft
          ]
        , [ binary "*" A.Multiplication AssocLeft
          , binary "/" A.Division AssocLeft
          ]
        , [ binary "+" A.Addition AssocLeft
          , binary "-" A.Subtraction AssocLeft
          ]
        , [ binary "++" A.StringConcatenation AssocLeft ]
        ]
  where
    binary name op = Infix
                       ( do
                         loc <- opLocation (reservedOp name)
                         return (binop_ loc op)
                       )
    unary name op = Prefix
                      ( do
                        loc <- opLocation (reservedOp name)
                        return (uop_ loc op)
                      )

opLocation :: Parser a -> Parser Location
opLocation p = do
            s <- getPosition
            _ <- p
            e <- getPosition
            return (newLoc s e)

var :: Parser (Expression Location)
var = addLoc1 Variable (liftM mkIdentifier identifier) <?> "variable"

term :: Parser (Expression Location)
term = parens expr
       <|> var
       <|> lit
       <?> "term"


lit :: Parser (Expression Location)
lit = addLoc1 Literal (slit <|> P.try mlit <|> ilit <|> blit) <?> "literal"

slit :: Parser (Literal Location)
slit = addLoc1 StringLiteral stringLiteral <?> "string literal"

mlit :: Parser (Literal Location)
mlit = addLoc1 MoneyLiteral  parseMoney <?> "money literal"

ilit :: Parser (Literal Location)
ilit = addLoc1 IntegerLiteral integer <?> "integer literal"

blit :: Parser (Literal Location)
blit = addLoc1 BooleanLiteral (true <|> false) <?> "bool literal"
  where
    true = reserved "true" >> return True
    false = reserved "false" >> return False

stmnt :: Parser (Statement Location)
stmnt = conditional <|> field <?> "statement"

conditional :: Parser (Statement Location)
conditional = P.try ifElseStmnt <|> ifStmnt <?> "conditional"

if' :: Parser (Expression Location, Block Location)
if' = do
  reserved "if"
  cond <- parens expr
  body <- block
  return (cond, body)

ifStmnt :: Parser (Statement Location)
ifStmnt = do
  s <- getPosition
  (cond, body) <- if'
  e <- getPosition
  return (If (newLoc s e) cond body) <?> "if statement"

ifElseStmnt :: Parser (Statement Location)
ifElseStmnt = do
  s <- getPosition
  (cond, firstBody) <- if'
  reserved "else"
  secondBody <- block
  e <- getPosition
  return (IfElse (newLoc s e) cond firstBody secondBody) <?> "if else statement"

form :: Parser (Form Location)
form = addLoc2 Form (reserved "form" *> liftM mkIdentifier identifier) (block <* eof) <?> "form"

parse :: String -> String -> Either ParseError (Form Location)
parse = P.parse form

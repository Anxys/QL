module QL.Value.ParseValue (parseValue, parseStringValue) where

import           QL.Money
import           QL.Value.ValueTypes
import           Text.ParserCombinators.Parsec          as P
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as Token

intValue :: Parser Value
intValue = do
  val <- integer
  eof
  return (IntValue val) <?> "int val"

moneyValue :: Parser Value
moneyValue = do
  val <- parseMoney
  eof
  return (MoneyValue val) <?> "money val"

stringValue :: Parser Value
stringValue = do
  val <- QL.Value.ParseValue.string
  eof
  return (StringValue val) <?> "string val"

value :: Parser Value
value = P.try moneyValue <|> intValue  <?> "value"

parse' :: Parser Value -> String -> Value
parse' p input = case P.parse p "parse value" input of
  Right val -> val
  Left  _ -> Undefined

parseValue :: String -> Value
parseValue = parse' value

parseStringValue :: String -> Value
parseStringValue = parse' stringValue

lexer :: Token.TokenParser st
lexer = Token.makeTokenParser emptyDef

integer :: Parser Integer
integer = Token.integer lexer

string :: Parser String
string = many anyChar <?> "string"

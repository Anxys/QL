module QL.Value.Value (module QL.Value.Value, module QL.Value.ValueTypes )
       where

import           QL.Language.Syntax.Ast
import           Control.Exception.Base
import           QL.Value.ParseValue
import           QL.Value.ValueTypes
import           Data.Data 

haveSameValueType :: Value -> Value -> Bool
haveSameValueType x y = toConstr x == toConstr y

andValues :: [Value] -> Bool
andValues = all toBoolValue 
  where toBoolValue (BoolValue x) = x
        toBoolValue _ = False 

defaultVal :: FieldType -> Value
defaultVal Integer = IntValue 0
defaultVal Boolean = BoolValue False
defaultVal String = StringValue ""
defaultVal Money = mkMoney 

toDisplay :: Value -> String
toDisplay (IntValue x) = show x
toDisplay (StringValue x) = x
toDisplay (MoneyValue x) = show x
toDisplay (BoolValue x) = show x 
toDisplay Undefined = "Undefined"

fromDisplay :: FieldType -> String -> Value
fromDisplay String = parseStringValue 
fromDisplay _  = parseValue 

neg :: Value -> Maybe Value
neg (BoolValue v) = Just (BoolValue $ not v)
neg _ = Nothing 

extractBoolValue :: Value -> Bool
extractBoolValue (BoolValue x) = x
extractBoolValue  _ = assert False (error "None boolean value supplied")

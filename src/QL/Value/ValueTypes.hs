{-# LANGUAGE DeriveDataTypeable #-}
module QL.Value.ValueTypes 
  where

import           Data.Data
import           QL.Money

data Value = IntValue Integer
           | BoolValue Bool
           | StringValue String
           | MoneyValue Money
           | Undefined
  deriving (Eq, Show, Data, Typeable)

mkMoney :: Value
mkMoney = MoneyValue newMoneyValue   

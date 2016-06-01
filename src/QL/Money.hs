{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module QL.Money (Money,parseMoney, doubleToMoney2, newMoneyValue, integerToMoney) where
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token    as P
import           Data.Data
import           Data.Decimal

deriving instance Data Decimal -- This is necessary for the usage of uniplate

newtype Money = Money Decimal
  deriving (Eq,Data, Typeable, Ord, Num)

instance Show Money
 where show (Money x) = show x

newMoneyValue :: Money
newMoneyValue =  Money (Decimal 2 000)

doubleToMoney2 :: Double -> Money
doubleToMoney2 val = Money (realFracToDecimal (decimalMantissa 2) val)

integerToMoney :: Integer -> Money
integerToMoney = Money . Decimal 0

parseMoney :: GenParser Char st Money
parseMoney = do sign <- option 1 (do s <- oneOf "+-"
                                     return $ if s == '-' then-1.0 else 1.0)
                x    <- P.float $ P.makeTokenParser emptyDef
                return $ doubleToMoney2 (sign * x)

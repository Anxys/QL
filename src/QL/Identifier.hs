{-# LANGUAGE DeriveDataTypeable #-}
module QL.Identifier (Identifier, mkIdentifier) where

import           Data.Data

newtype Identifier = Identifier String
  deriving (Ord, Eq, Data)

instance Show Identifier
  where show (Identifier x) = x

mkIdentifier :: String -> Identifier
mkIdentifier = Identifier

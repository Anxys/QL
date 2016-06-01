{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module QL.Location (
    Location(Location),
    Position(Position),
    startLine,
    startColumn,
    endLine,
    endColumn,
    ) where

import           Data.Data

data Position = Position { line :: Int, column :: Int }
  deriving (Eq, Typeable, Data)

instance Show Position
 where show x = "(line: " ++ show (line x) ++ ", column: " ++ show (column x)  ++ ")"

data Location = Location { start :: Position, end :: Position }
  deriving (Eq, Typeable, Data)

instance Show Location
 where show x = "(location {" ++ show (start x) ++ " - " ++ show (end x)  ++ "})"

startLine :: Location -> Int
startLine loc = line $ start loc

startColumn :: Location -> Int
startColumn loc = column $ start loc

endLine :: Location -> Int
endLine loc = line $ end loc

endColumn :: Location -> Int
endColumn loc = column $ end loc

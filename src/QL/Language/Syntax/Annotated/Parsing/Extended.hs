module QL.Language.Syntax.Annotated.Parsing.Extended where

import           QL.Location
import           Text.ParserCombinators.Parsec as P

newPos :: SourcePos -> Position
newPos s = Position (sourceLine s)  (sourceColumn s)

newLoc :: SourcePos -> SourcePos -> Location
newLoc s e = Location (newPos s) (newPos e)

addLoc0 :: (Location -> b Location) -> Parser () -> Parser (b Location)
addLoc0 f p = do
  s <- getPosition
  p
  e <- getPosition
  return (f (newLoc s e) )

addLoc1 :: (Location -> a -> b Location) -> Parser a -> Parser (b Location)
addLoc1 f p = do
  s <- getPosition
  v <- p
  e <- getPosition
  return (f (newLoc s e) v)

addLoc2 :: (Location -> a -> c -> b Location) -> Parser a  -> Parser c-> Parser (b Location)
addLoc2 f p1 p2 = do
  s <- getPosition
  v <- p1
  c <- p2
  e <- getPosition
  return (f (newLoc s e) v c)

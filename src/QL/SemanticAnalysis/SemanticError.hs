module QL.SemanticAnalysis.SemanticError
  where

import           QL.Identifier
import           QL.Language.Syntax.Ast as S (FieldType)
import           QL.Location            (Location)

data SemanticError = DuplicationIssue DuplicationIssue
                   | TypeError TypeError
                   | DependencyError DependencyError
  deriving (Eq)

type LeftType = S.FieldType

type RightType = S.FieldType

instance Show DuplicationIssue
 where
   show = showDuplicationIssue

instance Show TypeError
 where
   show = showTypeError

instance Show DependencyError
 where
   show = showDependencyError

instance Show SemanticError
 where
   show (DuplicationIssue x) = show x
   show (TypeError x) = show x
   show (DependencyError x) = show x

data DuplicationIssue = DuplicateIdentifier Identifier [Location]
                      | RedeclarationError Identifier Location [Location]
  deriving (Eq)

showDuplicationIssue :: DuplicationIssue -> String
showDuplicationIssue (DuplicateIdentifier identifier duplicateLocations) = "The identifier: " ++ show identifier ++ "," ++ " has been declared multiple times at the following locations:" ++ show duplicateLocations
showDuplicationIssue (RedeclarationError identifier _ duplicateLocations) = "The identifier: " ++ show identifier ++ " has been redeclared with different types at the following locations: " ++ show duplicateLocations

data TypeError = UndeclaredVariable Location
               | TypeMismatch LeftType RightType Location
  deriving (Eq )

showTypeError :: TypeError -> String
showTypeError (UndeclaredVariable location) = "There is a reference to an undeclared variable at " ++ show location
showTypeError (TypeMismatch lhs rhs location) = "There is a type error at " ++ show location ++ ":" ++ " (" ++ show lhs ++ " and " ++ show rhs ++ ")."

data DependencyError = CyclicDependencyError Identifier Location
                     | PostDependencyError (Identifier, Location) (Identifier, [Location])
  deriving (Eq)

showDependencyError :: DependencyError -> String
showDependencyError (CyclicDependencyError identifier location) = "There is a cyclic dependency for identifier: " ++ show identifier ++ " at " ++ show location ++ "."
showDependencyError (PostDependencyError (dependant, dependantLoc) (dependency, dependencyLoc)) = "The identifier: " ++ show dependant ++ " at " ++ show dependantLoc ++ " relies on the identifier: " ++ show dependency ++ " defined after it at " ++ show dependencyLoc ++ "."

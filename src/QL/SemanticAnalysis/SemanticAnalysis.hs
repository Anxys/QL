module QL.SemanticAnalysis.SemanticAnalysis (
    SemanticWarning,
    semanticCheck,
    hasNoErrors,
    toSemanticError,
    getWarnings,
    getType,
    module QL.SemanticAnalysis.SemanticError,
    ) where

import           Control.Exception.Base                    (assert)
import qualified Data.List                                 as L
import           QL.Identifier
import           QL.Language.Syntax.Annotated.AnnotatedAst as A
import           QL.Language.Syntax.Ast                    as S (FieldType (Integer, Money, String, Boolean))
import           QL.Location                               (Location)
import           QL.SemanticAnalysis.SemanticError

type TypeMap = (Identifier, S.FieldType)

data SemanticResult =
       SemanticResult
         { typeErrors          :: [TypeError]
         , cycleErrors         :: [DependencyError]
         , duplicationErrors   :: [DuplicationIssue]
         , duplicationWarnings :: [DuplicationIssue]
         }
  deriving (Eq, Show)

newtype SemanticWarning = SemanticWarning DuplicationIssue

instance Show SemanticWarning
  where show (SemanticWarning x) = show x

getWarnings :: SemanticResult -> [SemanticWarning]
getWarnings x = map SemanticWarning (duplicationWarnings x)

hasNoErrors :: SemanticResult -> Bool
hasNoErrors a = (null . cycleErrors) a && (null . typeErrors) a && (null . duplicationErrors) a

toSemanticError :: SemanticResult -> [SemanticError]
toSemanticError x = map DuplicationIssue (duplicationErrors x)
     ++ map DependencyError (cycleErrors x)
        ++ map TypeError (typeErrors x)

semanticCheck :: Form Location -> SemanticResult
semanticCheck x = uncurry (SemanticResult tErrors depErrors) dIssues
  where
    tErrors = checkForTypeErrors x
    depErrors = cErrors ++ checkForPostDependencyErrors x
    cErrors = checkForCyclicDependencyErrors ((transitiveClosure . getIdentifierRelation) calcFields) calcFields
    calcFields = collectCalculatedFields x
    fields = collectFields x
    dErrors = checkForDuplicationIssues fields
    dIssues = L.partition (not.isWarning) dErrors
    isWarning issue = case issue of
        DuplicateIdentifier{} -> True
        RedeclarationError{} -> False

checkForDuplicationIssues :: [Field Location] -> [DuplicationIssue]
checkForDuplicationIssues xs = map issue groups
  where
    dups = findDuplicates xs
    issue p = if sameType p
                  then DuplicateIdentifier (getFirstIdentifier p) (map getLoc p)
                  else RedeclarationError (getFirstIdentifier p) ((getLoc . head) p) (map getLoc p)
    sameType ys = all (== getT (head ys)) (map getT (tail ys))
    getT = toSimpleType.A.fieldType.extractFieldInfo
    groups = L.groupBy haveSameIdentifier dups
    getLoc = extractAnnotationFromField
    getFirstIdentifier = A.id.extractFieldInfo.head

findDuplicates :: [Field Location] -> [Field Location]
findDuplicates xs = concat (getDups groups)
  where
    groups = L.groupBy haveSameIdentifier xs
    getDups = filter (\x -> length x > 1)

checkForCyclicDependencyErrors :: [(Identifier, Identifier)] -> [Field Location] -> [DependencyError]
checkForCyclicDependencyErrors [] _ = []
checkForCyclicDependencyErrors (x:xs) env = checkForCycles x ++ checkForCyclicDependencyErrors xs env
  where
    checkForCycles (y, z) = if (y, y) `elem` (x : xs)
                              then map (uncurry CyclicDependencyError) (findIdent (y, z))
                              else []
    env' = map (\(CalculatedField loc info _) -> (A.id info, loc)) env
    findIdent q = filter (\(i, _) -> i == fst q) env'

transitiveClosure :: Eq a => [(a, a)] -> [(a, a)]
transitiveClosure closure
  | closure == closureUntilNow = closure
  | otherwise = transitiveClosure closureUntilNow
  where
    closureUntilNow =
      L.nub $ closure ++ [(a, c) | (a, b) <- closure
                                , (b', c) <- closure
                                , b == b']

checkForPostDependencyErrors :: Form Location -> [DependencyError]
checkForPostDependencyErrors form = L.nub $ concatMap (\x -> map (PostDependencyError (getIdentifier x, head (getLocations (getIdentifier x))).getDeclarationLocations) (postDeclarations x))  calcFields
  where extractLocationFromField = extractAnnotationFromField
        calcFields = collectCalculatedFields form
        fields = collectFields form
        getDeclarationLocations y = (y, getLocations y)
        getLocations x = map extractLocationFromField (filter ((x ==).getIdentifier) fields)
        identifiers = map getIdentifier fields
        dependencies x = map snd (dependencyRelation (toIdentifierExpr x))
        postDeclarations x = filter (isPostDeclaration (getIdentifier x)) (dependencies x)
        isPostDeclaration x y = minimum (L.elemIndices x identifiers) < minimum (L.elemIndices y identifiers)

dependencyRelation :: (Identifier, Expression a) -> [(Identifier, Identifier)]
dependencyRelation (varName, Variable _ name) = [(varName, name)]
dependencyRelation (_, Literal _ _) =
  []
dependencyRelation (varName, UnaryOperation _ _ rhs) =
  dependencyRelation (varName, rhs)
dependencyRelation (varName, BinaryOperation _ _ lhs rhs) =
  dependencyRelation (varName, lhs) ++ dependencyRelation (varName, rhs)

toIdentifierExpr :: Field a -> (Identifier, Expression a)
toIdentifierExpr (CalculatedField _ info expr) = (A.id info, expr)
toIdentifierExpr (SimpleField _ _) = assert False (error "Attempted to get values for SimpleFields")

getIdentifierRelation :: [Field Location] -> [(Identifier, Identifier)]
getIdentifierRelation  = foldr ((++) . dependencyRelation .toIdentifierExpr) []

checkForTypeErrors :: Form Location -> [TypeError]
checkForTypeErrors form@(Form _ _ ss) =
  typeCheckStatement ss
  where
    types = collectFormTypeMap form
    getT = toSimpleType . A.fieldType
    typeCheckStatement =
      concatMap typeCheckStatement'
    typeCheckStatement' (If loc expr ifblock) =
      case getType types expr of
        Left e ->
          e
        Right x -> if x == S.Boolean      -- Gotta be bools
                     then [] ++ typeCheckStatement ifblock
                     else [TypeMismatch x S.Boolean loc]
    typeCheckStatement' (IfElse loc expr ifblock elseblock) =
      case getType types expr of
        Left e ->
          e
        Right x -> if x == S.Boolean      -- Gotta be bools
                     then [] ++ typeCheckStatement ifblock ++ typeCheckStatement elseblock
                     else [TypeMismatch x S.Boolean loc]
    typeCheckStatement' (Field _ (SimpleField _ _))
     =
      []
    typeCheckStatement' (Field _ (CalculatedField loc info expr))
     =
      case getType types expr of
        Left e ->
          e
        Right x -> if x == getT info then -- The expression type should match the one declared
          [] else [TypeMismatch x (getT info) loc]

getType :: [TypeMap] -> Expression Location -> Either [TypeError] S.FieldType
getType _ (Literal _ (IntegerLiteral _ _)) =
  Right S.Integer
getType _ (Literal _ (MoneyLiteral _ _)) =
  Right S.Money
getType _ (Literal _ (StringLiteral _ _)) =
  Right S.String
getType _ (Literal _ (BooleanLiteral _ _)) =
  Right S.Boolean
getType types (Variable loc name) =
  case lookup name types of
    Nothing ->
      Left [UndeclaredVariable loc]
    Just a ->
      Right a
getType types (UnaryOperation _ _ rhs) = getType types rhs
getType types expr@(BinaryOperation _ _ lhs rhs) =
  getBinType expr rType lType
  where
    rType =
      getType types rhs
    lType =
      getType types lhs

getBinType :: Expression Location -> Either [TypeError] S.FieldType -> Either [TypeError] S.FieldType -> Either [TypeError] S.FieldType
getBinType BinaryOperation{} (Left lhs) (Left rhs) =
  Left (rhs ++ lhs)
getBinType BinaryOperation{} (Left lhs) _ =
  Left lhs
getBinType BinaryOperation{} _ (Left rhs) =
  Left rhs
getBinType (BinaryOperation loc op _ _) (Right lhs) (Right rhs) =
  case lookup (lhs,rhs) (opResultTypes op) of
    Nothing -> Left [TypeMismatch lhs rhs loc]
    Just resType -> Right resType
getBinType _ _ _  = assert False (error "Called with something that isn't a binary expression")

opResultTypes  :: BinaryOperation a -> [((S.FieldType, S.FieldType),S.FieldType)]
opResultTypes (A.Addition _)            =  [ ((S.Integer, S.Money),S.Money)
                                           , ((S.Integer, S.Integer), S.Integer)
                                           , ((S.Money, S.Integer),S.Money)
                                           , ((S.Money, S.Money),S.Money)
                                           ]
opResultTypes (A.Subtraction _)         =  [ ((S.Integer, S.Money),S.Money)
                                           , ((S.Integer, S.Integer), S.Integer)
                                           , ((S.Money, S.Integer),S.Money)
                                           , ((S.Money, S.Money),S.Money)
                                           ]
opResultTypes (A.Multiplication _)      =  [ ((S.Integer, S.Money),S.Money)
                                           , ((S.Integer, S.Integer),S.Integer)
                                           , ((S.Money, S.Integer),S.Money)
                                           ]
opResultTypes (A.Division _)            =  [ ((S.Integer, S.Money),S.Money)
                                           , ((S.Integer, S.Integer),S.Integer)
                                           , ((S.Money, S.Integer),S.Money)
                                           ]
opResultTypes (A.And _)                 =  [ ((S.Boolean, S.Boolean),S.Boolean) ]
opResultTypes (A.Or _)                  =  [ ((S.Boolean, S.Boolean),S.Boolean) ]
opResultTypes (A.GreaterThanOrEquals _) =  [ ((S.Integer, S.Money),S.Boolean)
                                           , ((S.Integer, S.Integer),S.Boolean)
                                           , ((S.Money, S.Integer),S.Boolean)
                                           , ((S.Money, S.Money),S.Boolean)
                                           ]
opResultTypes (A.GreaterThan _)         =  [ ((S.Integer, S.Money),S.Boolean)
                                           , ((S.Integer, S.Integer),S.Boolean)
                                           , ((S.Money, S.Integer),S.Boolean)
                                           , ((S.Money, S.Money),S.Boolean)
                                           ]
opResultTypes (A.LesserThanOrEquals _)  =  [ ((S.Integer, S.Money),S.Boolean)
                                           , ((S.Integer, S.Integer),S.Boolean)
                                           , ((S.Money, S.Integer),S.Boolean)
                                           , ((S.Money, S.Money),S.Boolean)
                                           ]
opResultTypes (A.LesserThan _)          =  [ ((S.Integer, S.Money),S.Boolean)
                                           , ((S.Integer, S.Integer),S.Boolean)
                                           , ((S.Money, S.Integer),S.Boolean)
                                           , ((S.Money, S.Money),S.Boolean)
                                           ]
opResultTypes (A.StringConcatenation _) =  [ ((S.String, S.String),S.String) ]
opResultTypes (A.NotEquals _)           =  [ ((S.Integer, S.Integer),S.Boolean)
                                           , ((S.Money, S.Money),S.Boolean)
                                           , ((S.Boolean, S.Boolean),S.Boolean)
                                           , ((S.String, S.String), S.Boolean)
                                           ]
opResultTypes (A.Equals _)              =  [ ((S.Integer, S.Integer),S.Boolean)
                                           , ((S.Money, S.Money),S.Boolean)
                                           , ((S.Boolean, S.Boolean),S.Boolean)
                                           , ((S.String, S.String), S.Boolean)
                                           ]

collectFormTypeMap :: Form Location -> [TypeMap]
collectFormTypeMap =
  collectTypeMap . collectFieldInformation

collectTypeMap :: [FieldInformation a] -> [TypeMap]
collectTypeMap =
  map (\y -> (A.id y, toSimpleType $ A.fieldType y))

toSimpleType :: A.FieldType a -> S.FieldType
toSimpleType (A.Money _) = S.Money
toSimpleType (A.Integer _) = S.Integer
toSimpleType (A.String _) = S.String
toSimpleType (A.Boolean _) = S.Boolean

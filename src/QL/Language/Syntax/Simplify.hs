module QL.Language.Syntax.Simplify (simplify, S.Form) where

import           QL.Language.Syntax.Annotated.AnnotatedAst as A
import           QL.Language.Syntax.Ast as S
import Control.Exception.Base

notSupportedError :: a
notSupportedError = assert False (error "Not supported")

simplify :: A.Form a -> S.Form
simplify = sForm

sLit :: A.Literal a -> Lit
sLit (A.IntegerLiteral _ x) = ILit x
sLit (A.MoneyLiteral _ x) = MLit x
sLit (A.StringLiteral _ x) = SLit x
sLit (A.BooleanLiteral _ x) = BLit x

sFieldType :: A.FieldType a -> S.FieldType
sFieldType (A.Money _) = S.Money
sFieldType (A.Integer _) = S.Integer
sFieldType (A.String _) = S.String
sFieldType (A.Boolean _) = S.Boolean

sExpr :: Expression a -> Expr
sExpr (Variable _ name) = Var name
sExpr (Literal _ x) = Lit $ sLit x
sExpr (BinaryOperation _ (LesserThan _) lhs rhs) = BinOp S.GT (sExpr rhs) (sExpr lhs)
sExpr (BinaryOperation _ (NotEquals _) lhs rhs) = UnOp S.Not (BinOp S.EQ (sExpr rhs) (sExpr lhs))
sExpr (BinaryOperation _ (LesserThanOrEquals _) lhs rhs) = BinOp --Desugar to GT and EQ
                                                             S.Or
                                                             (BinOp S.GT (sExpr rhs) (sExpr lhs))
                                                             (BinOp S.EQ (sExpr rhs) (sExpr lhs))
sExpr (BinaryOperation _ (GreaterThanOrEquals _) lhs rhs) = BinOp -- Desugar to GT and EQ
                                                              S.Or
                                                              (BinOp S.GT (sExpr lhs) (sExpr rhs))
                                                              (BinOp S.EQ (sExpr rhs) (sExpr lhs))
sExpr (BinaryOperation _ op lhs rhs) = BinOp (sBinOp op) (sExpr lhs) (sExpr rhs)
sExpr (UnaryOperation _ op rhs) = UnOp (sUnOp op) (sExpr rhs)

sUnOp :: UnaryOperation a -> UnOp
sUnOp (A.Not _) = S.Not

sBinOp :: BinaryOperation a -> BinOp
sBinOp (Addition _) = Add
sBinOp (Subtraction _) = Sub
sBinOp (Division _) = Div
sBinOp (Multiplication _) = Mul
sBinOp (A.Or _) = S.Or
sBinOp (StringConcatenation _) = SConcat
sBinOp (Equals _) = S.EQ
sBinOp (GreaterThan _) = S.GT
sBinOp (A.And _) = S.And
sBinOp (NotEquals _) = notSupportedError
sBinOp (GreaterThanOrEquals _) = notSupportedError
sBinOp (LesserThan _) = notSupportedError
sBinOp (LesserThanOrEquals _) = notSupportedError

sBlock :: A.Block a -> S.Block
sBlock [] = []
sBlock (A.IfElse _ expr b1 b2:xs) = S.If (sExpr expr) (sBlock b1) :
                                    S.If (UnOp S.Not (sExpr expr)) (sBlock b2) :
                                    sBlock xs -- Desugar If Else to two Ifs
sBlock (x:xs) = sStmnt x : sBlock xs

sStmnt :: Statement a -> Stmnt
sStmnt (A.Field _ x) = S.Field $ sField x
sStmnt (A.If _ expr block) = S.If (sExpr expr) (sBlock block)
sStmnt A.IfElse{} = notSupportedError

sField :: A.Field a -> S.Field
sField (A.SimpleField _ info) = S.SimpField $ sFieldInfo info
sField (A.CalculatedField _ info expr) = S.CalcField (sFieldInfo info) (sExpr expr)

sFieldInfo :: FieldInformation a -> FieldInfo
sFieldInfo x = FieldInfo (A.label x) (A.id x) (sFieldType $ A.fieldType x)

sForm :: A.Form a -> S.Form
sForm (A.Form _ name xs) = S.Form name (sBlock xs)

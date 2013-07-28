module Language.CatCat.Parser.LambdaParser where
import Language.CatCat.Parser.TypeParser
import Language.CatCat.Parser.Common 

-------------------
-- Syntax tree

newtype LambdaVarName = LambdaVarName { getLambdaVarName :: String }
  deriving (Show, Read, Eq)
data LambdaExpr t v 
  = LambdaForAll t (LambdaExpr t v)
  | LambdaArg v (Either t (TypeExpr t)) (LambdaExpr t v)
  | LambdaDefName DefinedName (LambdaExpr t v)

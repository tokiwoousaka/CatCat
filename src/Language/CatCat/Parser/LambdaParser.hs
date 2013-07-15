module Language.CatCat.Parser.LambdaParser where
import Language.CatCat.Parser.TypeParser

-------------------
-- Syntax tree

data Term t v = LambdaTerm (LambdaExpr t v) | TypeExpr t

newtype LambdaVarName = LambdaVarName { getLambdaVarName :: String }
  deriving (Show, Read, Eq)
data LambdaExpr t v = LambdaExpr --TODO
 
  

{-# LANGUAGE DeriveFunctor #-}

module Language.CatCat.Parser.TypeParser where
import Language.CatCat.Parser.Common
import Control.Applicative
import Data.Monoid
import Text.Trifecta

-------------------
-- Syntax tree

newtype TypeVarName = TypeVarName { getTypeVarName :: String }
  deriving (Show, Read, Eq)
data TypeExpr a 
  = TypeForall a (TypeExpr a)
  | TypeArg a (TypeExpr a)
  | TypeVar a (TypeExpr a)
  | TypeNest (TypeExpr a) (TypeExpr a)
  | TypeNil
    deriving (Show, Read, Eq, Functor)

instance Monoid (TypeExpr a) where
  mempty = TypeNil
  x `mappend` y = x `typeAppend` y

typeAppend :: TypeExpr a -> TypeExpr a -> TypeExpr a
typeAppend (TypeForall x n) l = TypeForall x $ n `typeAppend` l
typeAppend (TypeArg    x n) l = TypeArg    x $ n `typeAppend` l
typeAppend (TypeVar    x n) l = TypeVar    x $ n `typeAppend` l
typeAppend (TypeNest   e n) l = TypeNest   e $ n `typeAppend` l
typeAppend TypeNil          l = l

-------------------
-- Parser
type BasicExpr = TypeExpr TypeVarName

-- [test case]
-- parseTest (typeExpression <* eof) "^a, b . Forall c . a -> b -> c"
-- parseTest (typeExpression <* eof) "Forall a, b . a -> (a -> b) -> b"

commaSepSyn :: String -> (TypeVarName -> BasicExpr -> BasicExpr) -> Parser BasicExpr
commaSepSyn s c = let
  pvar = c <$> (TypeVarName <$> lWord) <*> pure TypeNil
  in symbol s *> (mconcat <$> commaSep1 pvar)

typeForall :: Parser BasicExpr
typeForall = commaSepSyn "Forall " TypeForall

typeArg :: Parser BasicExpr
typeArg = commaSepSyn "^" TypeArg

typeVar :: Parser BasicExpr
typeVar = TypeVar <$> (TypeVarName <$> lWord) <*> pure TypeNil

typeNest :: Parser BasicExpr
typeNest = TypeNest <$> parens typeExpression <*> pure TypeNil

typeExpression :: Parser BasicExpr
typeExpression = let
  syns = typeArg <|> typeForall <|> typeVar <|> typeNest
  arrs = symbol "->" <|> symbol "."
  in mconcat <$> sepBy syns arrs


-- | shorthand for building types and checking results in tests
module Helpers (
    tv,
    var,
    int,
    bool,
    str,
    char,
    strLit,
    (~>),
    list,
    assertLeft,
) where

import Data.Either (isLeft)
import Hmmm.Syntax (Expr (..), Literal (..), Name)
import Hmmm.Types
import Test.Tasty.HUnit

tv :: Name -> TypeVar
tv = TV

var :: Name -> TypeMaybe
var = TVar . tv

int, bool, char, str :: TypeMaybe
int = TLit CInt
bool = TLit CBool
char = TLit CChar
str = list char

-- | a string literal, as the parser produces it: a list of chars
strLit :: String -> Expr
strLit = EList . map (ELit . LChar)

(~>) :: TypeMaybe -> TypeMaybe -> TypeMaybe
(~>) a b = TCon $ TCFun a b
infixr 5 ~>

list :: TypeMaybe -> TypeMaybe
list = TCon . TCList

assertLeft :: (Show b) => String -> Either a b -> Assertion
assertLeft msg e = assertBool (msg ++ ", got " ++ either (const "Left") show e) (isLeft e)

-- | shorthand for building types and checking results in tests
module Helpers (
    tv,
    var,
    int,
    bool,
    str,
    (~>),
    assertLeft,
) where

import Data.Either (isLeft)
import Hmmm.Syntax (Name)
import Hmmm.Types
import Test.Tasty.HUnit

tv :: Name -> TypeVar
tv = TV

var :: Name -> TypeMaybe
var = TVar . tv

int, bool, str :: TypeMaybe
int = TLit CInt
bool = TLit CBool
str = TLit CStr

(~>) :: TypeMaybe -> TypeMaybe -> TypeMaybe
(~>) a b = TCon $ TCFun a b
infixr 5 ~>

assertLeft :: (Show b) => String -> Either a b -> Assertion
assertLeft msg e = assertBool (msg ++ ", got " ++ either (const "Left") show e) (isLeft e)

module TypeCheckerUtils where

import Data.List

import AbsLatte

areArgsUnique :: [Arg' a] -> Bool
areArgsUnique args = length args == length (nub $ getArgsIdents args)

getArgsIdents args = getArgsIdentsFoo args []

getArgsIdentsFoo [] acc = acc
getArgsIdentsFoo ((Arg _ _ (Ident i)):args) acc = getArgsIdentsFoo args (i:acc)

validArgsTypes ((Arg _ (Void _) _):_) = False
validArgsTypes (_:args) = True && validArgsTypes args
validArgsTypes [] = True

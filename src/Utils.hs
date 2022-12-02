module Utils where

import AbsLatte


prologue :: String
prologue = unlines [ "declare void @printInt(i32)"
    , "declare void @printString(i8*)"
    , "declare void @error()"
    , "declare i32 @readInt()"
    , "declare i8* @readString()"
    , "declare i8* @__concatString__(i8*, i8*)"
    , "declare i1 @__cmpString__(i8*, i8*)"
    , "declare i8* @__llvmMemcpy__(i32)"]

fromJust :: Maybe a -> a
fromJust Nothing  = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x

showIdent :: Ident -> String
showIdent (Ident a) = " (Variable: " ++ a ++ ")"

dropPointer :: String -> String
dropPointer [] = error "Empty list!"
dropPointer ['*'] = []
dropPointer [a] = [a]
dropPointer (h:t) = [h] ++ dropPointer t
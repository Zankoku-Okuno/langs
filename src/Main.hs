{-#LANGUAGE FlexibleInstances #-}
module Main where

import Data.List
import qualified AList as A

import Ident
import Const
import F.Parser
import F.Syntax
import F.Typecheck

import Text.Luthor
import Text.Luthor.Syntax
import Text.Parsec.Combinator (eof)
import System.Environment

instance Arr String where
    mkArr = "->"
    isArr = (== "->")


main = do
    files <- getArgs
    aFile `mapM_` files
    --putStrLn "Goodbyte, cruel world!"

aFile filename = do
    contents <- readFile filename
    case compile contents filename of
        Left err -> putStrLn err
        Right (e, t) -> print e >> print t

compile :: String -> SourceName -> Either String (Term String String Id, Type String Id)
compile source from = 
    case parse parser from source of
        Left err -> Left $ show err
        Right e -> case runScope $ scopeCheck e of
            Left err -> Left $ "Free variables: " ++ intercalate ", " (show <$> err)
            Right e -> case typeCheck ctx0 e of
                Nothing -> Left "type error"
                Just t -> Right (e, t)
    where
    parser = (expr <* many (void lws <||> newline) <* eof)

ctx0 :: Context String String Id
ctx0 = Ctx A.empty A.empty
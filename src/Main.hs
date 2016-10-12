{-#LANGUAGE FlexibleInstances #-}
module Main where

import Data.List
import AList

import Ident
import Const
import F.Syntax
import F.Parser

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
        Right e -> print e

compile :: String -> SourceName -> Either String (Term String String Id)
compile source from = 
    case parse parser from source of
        Left err -> Left $ show err
        Right e -> case runScope $ scopeCheck e of
            Left err -> Left $ "Free variables: " ++ intercalate ", " (show <$> err)
            Right e -> Right e
    where
    parser = (expr <* many (void lws <||> newline) <* eof)
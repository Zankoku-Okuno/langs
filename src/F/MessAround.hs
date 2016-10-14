{-#LANGUAGE FlexibleInstances #-}
module F.MessAround where


import Const
import F.Parser
import F.Syntax
import F.Scope as Scope
import F.Typecheck as Tc
import F.Print

import Data.List (intercalate)
import Text.Luthor
import Text.Luthor.Syntax
import Text.Parsec.Combinator (eof)
import System.Environment


instance ArrC String where
    mkArr = "->"
    isArr = (== "->")


main = do
    files <- getArgs
    aFile `mapM_` files
    --putStrLn "Goodbyte, cruel world!"

aFile filename = do
    contents <- readFile filename
    putStrLn ""
    putStrLn $ filename ++ ":"
    putStrLn contents
    case compile contents filename of
        Left err -> putStrLn err
        Right (e, t) -> print e >> print t

-- FIXME kind check (really, make sure that type constructors are only given exactly the number of arguments that they have arity for)
compile :: String -> SourceName -> Either String
            ( Term () String String SourceId
            , Type () String SourceId
            )
compile source from =
    case parse parser from source of
        Left err -> Left $ show err
        Right e -> case scopeCheck scopeCtx0 e of
            errs@(_:_) -> Left $ "Free variables:\n\t" ++ intercalate "\n\t" (show <$> errs)
            [] -> case typeCheck tcCtx0 e of
                Nothing -> Left "type error"
                Just t -> Right (e, t)
    where
    parser = (expr <* many (void lws <||> newline) <* eof)

scopeCtx0 :: Scope.Context SourceId
scopeCtx0 = Scope.Ctx [] []
tcCtx0 :: Tc.Context () String String SourceId
tcCtx0 = Tc.Ctx [] []
{-#LANGUAGE FlexibleInstances,
            GADTs #-}
module F.MessAround where


import Data.String (IsString(..))

import F.Parser
import F.Syntax
import F.Scope as Scope
import F.Typecheck as Tc
import qualified Lambda.Syntax as Lambda
import qualified Lambda.Interpreter as Lambda
import F.CodeGen.Lambda

import F.Print
import Lambda.Print

import Data.List (intercalate)
import Text.Luthor
import Text.Luthor.Syntax
import Text.Parsec.Combinator (eof)
import System.Environment


import F.Unify



main = do
    files <- getArgs
    aFile `mapM_` files

aFile filename = do
    contents <- readFile filename
    putStrLn ""
    putStrLn $ filename ++ ":"
    putStrLn contents
    case compile contents filename of
        Left err -> putStrLn err
        Right (e, e', t) -> do
            print e >> print t >> print e'
            let delta _ _ _ = error "no primitive functions for you!"
            res <- Lambda.eval delta e'
            case res of
                Left stuck -> putStrLn $ "stuck: " ++ show stuck
                Right v -> print v

compile :: String -> SourceName -> Either String
            ( Term () (C String) (Id StrId)
            , Lambda.Term () (C String) (Id StrId)
            , Type () (C String) (Id StrId)
            )
compile source from =
    case parse parser from source of
        Left err -> Left $ show err
        Right e -> case scopeCheck scopeCtx0 e of
            errs@(_:_) -> Left $ "Free variables:\n\t" ++ intercalate "\n\t" (show <$> errs)
            [] -> case typeCheck tcCtx0 e of
                Left errs -> Left $ intercalate "\n" (show <$> errs)
                Right t -> Right (e, codegen e, t)
    where
    parser = (expr <* eof)

scopeCtx0 :: Scope.Context (Id StrId)
scopeCtx0 = Scope.Ctx [] []
tcCtx0 :: Tc.Context () (C String) (Id StrId)
tcCtx0 = Tc.Ctx [] (const Nothing) (flip lookup [(TypeC "->", 2), (TypeC "Int", 0)])





instance ArrC (C String TypeLevel) where
    mkArr = TypeC "->"
    isArr = (== TypeC "->")
instance IsString (C String TypeLevel) where
    fromString = TypeC
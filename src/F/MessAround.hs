{-#LANGUAGE FlexibleInstances, MultiParamTypeClasses, GADTs #-}
module F.MessAround where


import System.Environment
import Data.String (IsString(..))
import Data.List (intercalate, isSuffixOf)

import Context
import F.Parser
import F.Syntax
import F.Scope as Scope
import F.Typecheck as Tc
import qualified Lambda.Syntax as Lambda
import qualified Lambda.Interpreter as Lambda
import F.CodeGen.Lambda

import F.Print
import Lambda.Print

import Text.Luthor
import Text.Luthor.Syntax
import Text.Parsec.Combinator (eof)

import F.Unify



main = do
    files <- filter (".f" `isSuffixOf`) <$> getArgs
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
        Right e -> case typeCheck ctx0 e of
            Left errs -> Left $ intercalate "\n" (show <$> errs)
            Right t -> Right (e, codegen e, t)
    where
    parser = (expr <* eof)


ctx0 :: Context () (C String) (Id StrId) Type (Paramd Int) Nada
ctx0 = Ctx
    { gamma = mempty
    , constants = mempty { typeConstants = flip lookup
            [ (TypeC "->", Paramd 2)
            , (TypeC "Int", Paramd 0)
            ]
        }
    }





instance ArrC attr (Type attr (C String) id) where
    toArr attr a b = Ctor attr (TypeC "->") [a, b]
    fromArr (Ctor attr (TypeC "->") [a, b]) = Just (attr, a, b)
    fromArr _ = Nothing
instance IsString (C String TypeLevel) where
    fromString = TypeC
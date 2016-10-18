{-#LANGUAGE FlexibleInstances, MultiParamTypeClasses, GADTs #-}
module FOmegaNoLambda.MessAround where

import System.Environment
import Data.String (IsString(..))
import Data.List (intercalate, isSuffixOf)

import Context
import FOmegaNoLambda.Parser
import FOmegaNoLambda.Syntax
import FOmegaNoLambda.Scope
import FOmegaNoLambda.Typecheck
import FOmegaNoLambda.Print
import FOmegaNoLambda.CodeGen.Lambda
import qualified Lambda.Syntax as Lambda
import qualified Lambda.Interpreter as Lambda
import qualified Lambda.Print as Lambda

import Text.Luthor
import Text.Luthor.Syntax
import Text.Parsec.Combinator (eof)


main = do
    files <- filter (".fo" `isSuffixOf`) <$> getArgs
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
            --Right t -> Right (e, (), t)
            Right t -> Right (e, codegen e, t)
    where
    parser = (expr <* eof)


ctx0 :: Context () (C String) (Id StrId) Type Kind (Paramd Int)
ctx0 = Ctx
    { gamma = mempty
    , constants = mempty
        { typeConstants = flip lookup
            [ (TypeC "->", Arr () unitKind (Arr () unitKind unitKind) )
            , (TypeC "Int", unitKind)
            ]
        , kindConstants = flip lookup
            [ (KindC "->", Paramd 2)
            , (KindC "*", Paramd 0)
            ]
        }
    }
    where
    unitKind = Ctor () (KindC "*") []





instance ArrC attr (Type attr (C String) id) where
    toArr attr a b = App attr (App attr (Const attr (TypeC "->")) a) b
    fromArr (App attr (App' (Const' (TypeC "->")) a) b) = Just (attr, a, b)
    fromArr _ = Nothing
instance ArrC attr (Kind attr (C String) id) where
    toArr attr a b = Ctor attr (KindC "->") [a, b]
    fromArr (Ctor attr (KindC "->") [a, b]) = Just (attr, a, b)
    fromArr _ = Nothing
instance IsString (C String TypeLevel) where
    fromString = TypeC
instance IsString (C String KindLevel) where
    fromString = KindC
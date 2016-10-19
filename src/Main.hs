{-#LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Main where

import Data.String (IsString(..))

import Syntax
import Lambda.Syntax
import Lambda.Scope
import Lambda.Interpreter
import Lambda.Print

import qualified F.MessAround
--import qualified FOmegaNoLambda.MessAround

term :: Term () (C Int) (Id StrId)
term = App () (App ()
           (Abs () "x" $ Abs () "x" $ Var () "x")
           (Const () (TermC 4)))
           (Const () (TermC 5))

main = do
    print term
    case scopeCheck term of
        [] -> print =<< eval delta term
        errs -> mapM_ (putStrLn . ("scope error: " ++)) (show <$> errs)
    F.MessAround.main
    --FOmegaNoLambda.MessAround.main


delta attr _ _ = putStrLn ("HA!" ++ show attr) >> pure Nothing

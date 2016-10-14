{-#LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Main where

import Data.String (IsString(..))

import Syntax
import Lambda.Syntax
import Lambda.Scope
import Lambda.Interpreter
import Lambda.Print

import qualified F.MessAround

term :: Term () Int SourceId
term = App () (App () (Abs () "x" $ Abs () "x" $ Var () "x") (Const () 4)) (Const () 5)

main = do
    print term
    case scopeCheck term of
        [] -> print =<< eval delta term
        errs -> mapM_ (putStrLn . ("scope error: " ++)) (show <$> errs)
    F.MessAround.main


delta attr _ _ = putStrLn ("HA!" ++ show attr) >> pure Nothing



instance IsString (SourceId TermLevel) where
    fromString = TermId
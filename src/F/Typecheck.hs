{-#LANGUAGE FlexibleContexts #-}
module F.Typecheck where

import AList (AList)
import qualified AList as A

import Const
import Ident
import F.Syntax


data Context c tc id = Ctx (AList id (Type tc id)) (AList c (Type tc id))

typeofVar :: (Eq id) => Context c tc id -> id -> Maybe (Type tc id)
typeofVar (Ctx xs _) x = xs `A.lookup` x

typeofCon :: (Eq c) => Context c tc id -> c -> Maybe (Type tc id)
typeofCon (Ctx _ cs) c = cs `A.lookup` c

bindVar :: (Eq id) => Context c tc id -> (id, Type tc id) -> Context c tc id
bindVar (Ctx xs cs) (x, t) = Ctx (A.update xs x t) cs


typeCheck :: (Eq id, Eq (Type tc id), Eq c, Eq tc, Arr tc) => Context c tc id -> Term c tc id -> Maybe (Type tc id)
typeCheck ctx (Var x) = ctx `typeofVar` x
typeCheck ctx (Con c) = ctx `typeofCon` c
typeCheck ctx (Abs (x, t1) e) = do
    t2 <- typeCheck (ctx `bindVar` (x, t1)) e
    return $ ArrTy t1 t2
typeCheck ctx (App e1 e2) = do
    tf <- typeCheck ctx e1
    t <- typeCheck ctx e2
    unifyFun tf t
typeCheck ctx (TyAbs a e) = do
    t <- typeCheck ctx e
    return $ Univ a t
typeCheck ctx (TyApp e t) = do
    sigma <- typeCheck ctx e
    inst sigma t


unifyFun :: (Eq (Type tc id), Arr tc) => Type tc id -> Type tc id -> Maybe (Type tc id)
unifyFun (ArrTy t1 t2) t1' | t1 == t1' = Just t2
unifyFun _ _ = Nothing

inst :: (Eq id) => Type tc id -> Type tc id -> Maybe (Type tc id)
inst (Univ a sigma) t = Just $ subst [(a, t)] sigma
inst _ _ = Nothing
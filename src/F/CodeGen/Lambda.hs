module F.CodeGen.Lambda where

import F.Syntax as FO
import Lambda.Syntax as Lambda

codegen :: FO.Term attr c id -> Lambda.Term attr c id
codegen (Var attr x) = Var attr x
codegen (Const attr c) = Const attr c
codegen (Abs attr (x, _) e) = Abs attr x (codegen e)
codegen (App attr e1 e2) = App attr (codegen e1) (codegen e2)
codegen (BigAbs' _ e) = codegen e
codegen (BigApp' e _) = codegen e
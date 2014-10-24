module Data.WithClass.Derive.DeepTypeable where

import Language.Haskell as H

import Data.Derive.Internal.Derivation
import Data.List

-- * Derive opaque @DeepTypeable@ instances

makeDeepTypeableAbstract :: Derivation
makeDeepTypeableAbstract = derivationCustom "DeepTypeable" $ \(q,d) -> Right $ makeDeepTypeableAbstractInstance q d

makeDeepTypeableAbstractInstance :: ModuleName -> DataDecl -> [Decl]
makeDeepTypeableAbstractInstance q d = [InstDecl sl Nothing [] [] (deepTypeableQual $ Ident "DeepTypeable") [sig] [dec]]
	where
	vars = dataDeclVars d
	sig = foldl (\ty var -> TyApp ty (TyVar $ Ident var)) (TyCon $ UnQual $ Ident $ dataDeclName d) vars
	dec = makeDeepTypeableTypeTree (dataDeclName d) sig (map (TyVar . Ident) vars) []

-- * Derive @DeepTypeable@ instances for algebraic data types

makeDeepTypeable ::  Derivation
makeDeepTypeable = derivationCustom "DeepTypeable" $ \(q,d) -> Right $ makeDeepTypeableInstance q d

makeDeepTypeableInstance :: ModuleName -> DataDecl -> [Decl]
makeDeepTypeableInstance q d = [InstDecl sl Nothing [] ctx (deepTypeableQual $ Ident "DeepTypeable") [sig] [dec]]
	where
	vars = dataDeclVars d
	ctors = dataDeclCtors d
	tys = nub $ ctorDeclsTypes ctors ++ (map (TyVar . Ident) vars)
	ctx = map (\ty -> ClassA (UnQual $ Ident "DeepTypeable") [ty]) tys
	sig = foldl (\ty var -> TyApp ty (TyVar $ Ident var)) (TyCon $ UnQual $ Ident $ dataDeclName d) vars
	dec = makeDeepTypeableTypeTree (dataDeclName d) sig (map (TyVar . Ident) vars) ctors

makeDeepTypeableTypeTree :: String -> Type -> [Type] -> [CtorDecl] -> InstDecl
makeDeepTypeableTypeTree name sig vars ctors = InsDecl $ FunBind [makeDeepTypeableTypeTree' name sig vars ctors]

makeDeepTypeableTypeTree' :: String -> Type -> [Type] -> [CtorDecl] -> Match
makeDeepTypeableTypeTree' name sig vars ctors = Match sl (Ident "typeTree") [mkProxyPat sig] Nothing (UnGuardedRhs expr) (BDecls [])
	where
	expr = app3 conMkTypeTree
		(App (Var $ Qual (ModuleName "Language.Haskell.TH.Syntax") $ Ident "mkName") $ Lit $ String name)
		(List $ map makeDeepTypeableTypeTreeArg vars)
		(List $ map (makeDeepTypeableTypeTreeCtor) ctors)

makeDeepTypeableTypeTreeCtor :: CtorDecl -> Exp
makeDeepTypeableTypeTreeCtor c = App (App conMkConTree (App (Var $ Qual (ModuleName "Language.Haskell.TH.Syntax") $ Ident "mkName") $ Lit $ String $ ctorDeclName c)) $ List $ map (makeDeepTypeableTypeTreeArg) $ ctorDeclTypes c

makeDeepTypeableTypeTreeArg :: Type -> Exp
makeDeepTypeableTypeTreeArg ty = App (Var $ UnQual $ Ident $ "typeTree") (ExpTypeSig sl (Con $ UnQual $ Ident "Proxy") (TyApp (TyCon $ UnQual $ Ident "Proxy") ty))

mkProxyPat :: H.Type -> H.Pat
mkProxyPat ty = PatTypeSig sl patProxy (TyApp (TyCon $ UnQual proxyN) ty)
	
proxyN :: Name
proxyN = Ident "Proxy"

patProxy :: Pat
patProxy = PApp (UnQual $ Ident "Proxy") []

conMkTypeTree :: Exp
conMkTypeTree = Con $ UnQual $ Ident "MkTypeTree"

conMkConTree :: Exp
conMkConTree = Con $ UnQual $ Ident "MkConTree"

deepTypeableQual :: Name -> QName
deepTypeableQual name = Qual (ModuleName "Data.DeepTypeable") name

ctorDeclTypes :: CtorDecl -> [Type]
ctorDeclTypes = map (snd) . ctorDeclFields

ctorDeclsTypes :: [CtorDecl] -> [Type]
ctorDeclsTypes = concatMap ctorDeclTypes

app3 x y z w = App (App (App x y) z) w

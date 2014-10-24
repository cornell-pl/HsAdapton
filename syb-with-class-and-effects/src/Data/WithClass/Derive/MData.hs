module Data.WithClass.Derive.MData (makeMDataAbstract,makeMData,ctorDeclsTypes,ctorDeclVars,app2,app4,mkCtorExp,mkCtorPat) where

import Language.Haskell as H

import Data.Derive.Internal.Derivation
import Data.List

-- * Derive opaque @MData@ instances (default @gfoldl@ and empty declarations for the remaining functions)

makeMDataAbstract :: Derivation
makeMDataAbstract = derivationCustom "MData" $ \(q,d) -> Right $ makeMDataAbstractInstance q d

makeMDataAbstractInstance :: ModuleName -> DataDecl -> [Decl]
makeMDataAbstractInstance q d = [InstDecl sl Nothing [] ctx (mdataQual $ Ident "MData") [ct,m,sig] []]
	where
	vars = dataDeclVars d
	ctors = dataDeclCtors d
	tys = nub $ ctorDeclsTypes ctors
	ctx = ClassA (UnQual $ Ident "Sat") [TyApp ct sig] : ClassA (UnQual $ Ident "Monad") [m] : concatMap (\var -> [ClassA (UnQual $ Ident "DeepTypeable") [TyVar $ Ident var]]) vars ++ map (\ty -> ClassA (UnQual $ Ident "MData") [ct,m,ty]) tys
	sig = foldl (\ty var -> TyApp ty (TyVar $ Ident var)) (TyCon $ UnQual $ Ident $ dataDeclName d) vars

-- * Derive @MData@ instances for algebraic data types

makeMData ::  Derivation
makeMData = derivationCustom "MData" $ \(q,d) -> Right $ makeMDataInstance q d

makeMDataInstance :: ModuleName -> DataDecl -> [Decl]
makeMDataInstance q d = [InstDecl sl Nothing [] ctx (mdataQual $ Ident "MData") [ct,m,sig] (makeMDataGfoldl ctors ++ makeMDataGunfold ctors ++ makeMDataToConstr ctors ++ makeMDataDataTypeOf q (dataDeclName d) ctors)]
	where
	vars = dataDeclVars d
	ctors = dataDeclCtors d
	tys = nub $ ctorDeclsTypes ctors
	ctx = ClassA (UnQual $ Ident "Sat") [TyApp ct sig]
		: ClassA (UnQual $ Ident "Monad") [m]
		: map (\ty -> ClassA (UnQual $ Ident "MData") [ct,m,ty]) tys
		++ concatMap (\var -> [ClassA (UnQual $ Ident "DeepTypeable") [TyVar $ Ident var]]) vars
	sig = foldl (\ty var -> TyApp ty (TyVar $ Ident var)) (TyCon $ UnQual $ Ident $ dataDeclName d) vars
	
--concatMap (\var -> [ClassA (mdataQual $ Ident "MData") [ct,m,TyVar $ Ident var],ClassA (UnQual $ Ident "Typeable") [TyVar $ Ident var]]) vars ++
	
makeMDataGfoldl :: [CtorDecl] -> [InstDecl]
makeMDataGfoldl ctors = [InsDecl $ FunBind $ map makeMDataGfoldl' ctors]

makeMDataGfoldl' :: CtorDecl -> Match
makeMDataGfoldl' c = Match sl (Ident "gfoldl") [PVar $ Ident "ctx",PVar $ Ident "k",PVar $ Ident "z",mkCtorPat c] Nothing (UnGuardedRhs expr) (BDecls [])
	where
	expr = foldl (\e var -> e `rightArrHighApp` (flipk `App` (returnExpr $ Var $ UnQual $ Ident var))) ((Var $ UnQual $ Ident "z") `App` makeFoldZFun c) cvars
	cvars = ctorDeclVars c
	flipk = Paren $ (Var $ UnQual $ Ident "flip") `App` (Var $ UnQual $ Ident "k")

makeFoldZFun c = foldr (\(mvar,var) e -> Paren $ Lambda sl [PVar $ Ident mvar] $ InfixApp (Var $ UnQual $ Ident mvar) (QVarOp $ UnQual $ Ident ">>=") $ Lambda sl [PVar $ Ident var] $ returnExpr e) (mkCtorExp c) (zip (ctorDeclMVars c) (ctorDeclVars c))

rightArrHighApp e1 e2 = InfixApp e1 (QVarOp $ UnQual $ Ident ">>=") e2
returnExpr e = App (Var $ UnQual $ Ident "return") e

makeMDataGunfold :: [CtorDecl] -> [InstDecl]
makeMDataGunfold ctors = [InsDecl $ FunBind [Match sl (Ident "gunfold") [PVar $ Ident "ctx",PVar $ Ident "k",PVar $ Ident "z",PVar $ Ident "c"] Nothing (UnGuardedRhs expr) (BDecls [])]]
	where
	expr = ((Var $ mdataQual $ Ident "constrIndex") `App` (Var $ UnQual $ Ident "c")) `Case` alts
	alts = map makeMDataGunfold' $ zip [1..] ctors

makeMDataGunfold' :: (Int,CtorDecl) -> Alt
makeMDataGunfold' (i,c) = Alt sl (PLit Signless $ Int $ toEnum i) (UnGuardedRhs expr) (BDecls [])
	where expr = foldl (\e var -> e `rightArrHighApp` (Var $ UnQual $ Ident "k")) ((Var $ UnQual $ Ident "z") `App` makeFoldZFun c) (ctorDeclVars c)

makeMDataToConstr :: [CtorDecl] -> [InstDecl]
makeMDataToConstr cs = [InsDecl $ FunBind $ map makeMDataToConstr' $ zip [1..] cs]

makeMDataToConstr' :: (Int,CtorDecl) -> Match
makeMDataToConstr' (i,c) = Match sl (Ident "toConstr") [PVar $ Ident "ctx",PAsPat (Ident "x") $ mkCtorPat c] Nothing (UnGuardedRhs expr) (BDecls [])
	where expr = (app2 (Var $ mdataQual $ Ident "dataTypeOf") (Var $ UnQual $ Ident "ctx") (Var $ UnQual $ Ident "x")) `rightArrHighApp` (InfixApp (Var $ UnQual $ Ident "return") (QVarOp $ UnQual $ Ident ".") (app2 (Var $ UnQual $ Ident "flip") (Var $ mdataQual $ Ident "indexConstr") (Lit $ Int $ toEnum i)))

makeMDataDataTypeOf :: ModuleName -> String -> [CtorDecl] -> [InstDecl]
makeMDataDataTypeOf (ModuleName q) dname cs = [InsDecl $ FunBind [Match sl (Ident "dataTypeOf") [PVar $ Ident "ctx",PVar $ Ident "x"] Nothing (UnGuardedRhs $ returnExpr $ Var $ UnQual $ Ident "ty") (BDecls [FunBind [Match sl (Ident "ty") [] Nothing (UnGuardedRhs expr) (BDecls [])]])]]
	where expr = app2 (Var $ mdataQual $ Ident "mkDataType") (Lit $ String $ q++"."++dname) (List $ map makeMDataDataTypeOf' cs)

makeMDataDataTypeOf' :: CtorDecl -> Exp
makeMDataDataTypeOf' c = app4 (Var $ mdataQual $ Ident "mkConstr") (Var $ UnQual $ Ident "ty") (Lit $ String $ ctorDeclName c) (List args) (Con $ UnQual $ Ident "Prefix")
	where args = []

mkCtorPat :: CtorDecl -> Pat
mkCtorPat c = PApp (UnQual $ Ident $ ctorDeclName c) args
	where args = map (\var -> PVar $ Ident var) (ctorDeclVars c)

mkCtorExp :: CtorDecl -> Exp
mkCtorExp c = Paren $ foldl (\e var -> App e $ Var $ UnQual $ Ident var) (Con $ UnQual $ Ident $ ctorDeclName c) (ctorDeclVars c)

ctorDeclVars :: CtorDecl -> [String]
ctorDeclVars c = map (\i -> "x"++show i) $ take (length $ ctorDeclFields c) [1..]

ctorDeclMVars :: CtorDecl -> [String]
ctorDeclMVars c = map (\i -> "mx"++show i) $ take (length $ ctorDeclFields c) [1..]
	
ct = TyVar $ Ident "ctx"
m = TyVar $ Ident "m"

mdataQual :: Name -> QName
mdataQual name = Qual (ModuleName "Data.WithClass.MData") name

dataQual :: Name -> QName
dataQual name = Qual (ModuleName "Data.Data") name
	
app2 x y z = App (App x y) z
app4 x y z w q = App (App (App (App x y) z) w) q

ctorDeclTypes :: CtorDecl -> [Type]
ctorDeclTypes = map (snd) . ctorDeclFields

ctorDeclsTypes :: [CtorDecl] -> [Type]
ctorDeclsTypes = concatMap ctorDeclTypes

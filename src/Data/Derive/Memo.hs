{-# LANGUAGE TemplateHaskell, ViewPatterns #-}

module Data.Derive.Memo where

import Language.Haskell.TH.Syntax as TH
import Data.Derive.Internal.Derivation
import Data.DeriveTH
import Data.Memo
import Data.Typeable
import Data.Hashable
import System.Mem.Weak.Exts
import System.Mem.StableName.Exts
import Control.Monad
import qualified Language.Haskell as H
import Language.Haskell.Convert
import Data.Generics

-- | creates a new unique datatype placeholder
deriveName :: String -> Q [Dec]
deriveName str = deriveName' (mkName str)

deriveName' :: Name -> Q [Dec]
deriveName' name = do
	let decl = DataD [] name [] [NormalC name []] [''Typeable,''Eq]
	let ty = ConT name
	let rty = return ty
	hashable <- [d|
		instance Hashable $rty where
			hash _ = 0
			hashWithSalt salt _ = salt |]
	memo <- [d|
		instance Memo $rty where
			type Key $rty = $rty
			{-# INLINE memoKey #-}
			memoKey = id
			{-# INLINE memoWeak #-}
			memoWeak = \x -> MkWeak $ mkWeak x |]
	return $ decl : hashable ++ memo

unKindTV (KindedTV n k) = PlainTV n
unKindTV t = t

-- | creates an identity @Memo@ instance
deriveMemoId :: Name -> Q [Dec]
deriveMemoId name = do
	x <- reify name
	case x of
		TyConI dec -> deriveMemoIdFromDec dec
		otherwise -> error "deriveMemoId"

deriveMemoIdFromDec :: Dec -> Q [Dec]
deriveMemoIdFromDec dec = do
	let name = decName dec
	let decl = convert $ everywhere (mkT unKindTV) dec
	let ty = foldl AppT (ConT name) (map (VarT . mkName) $ H.dataDeclVars decl)
	let rty = return ty
	[d|
		instance (Typeable $rty,Eq $rty,Hashable $rty) => Memo $rty where
			type Key $rty = $rty
			{-# INLINE memoKey #-}
			memoKey = id
			{-# INLINE memoWeak #-}
			memoWeak = \x -> MkWeak $ mkWeak x |]

-- | creates a default @Memo@ instance using @StableName@
deriveMemo :: Name -> Q [Dec]
deriveMemo name = do
	x <- reify name
	case x of
		TyConI dec -> deriveMemoFromDec dec
		otherwise -> error "deriveMemo"

deriveMemoFromDec :: Dec -> Q [Dec]
deriveMemoFromDec dec = do
	let name = decName dec
	let decl = convert $ everywhere (mkT unKindTV) dec
	let vars = map (VarT . mkName) $ H.dataDeclVars decl
	let ty = foldl AppT (ConT name) vars
	let rty = return ty
	[d|
		instance Typeable $rty => Memo $rty where
			type Key $rty = StableName $rty
			{-# INLINE memoKey #-}
			memoKey = stableName
			{-# INLINE memoWeak #-}
			memoWeak = \x -> MkWeak $ mkWeak x |]

deriveMemoNewType :: Dec -> Q [Dec]
deriveMemoNewType dec = do
	let (NewtypeD _ name args con _) = everywhere (mkT unKindTV) dec
	let vars = map (\(PlainTV n) -> VarT n) args
	let ty = foldl AppT (ConT name) vars
	let rty = return ty
	(destrE,innerty) <- case con of
		NormalC conname [(_,innerty)] -> do
			x <- newName "x"
			return (LamE [ConP conname [VarP x]] (VarE x),innerty)
		RecC conname [(destrname,_,innerty)] -> return (VarE destrname,innerty)
		otherwise -> error "deriveMemoNewType"
	let rdestrE = return destrE
	let rinnerty = return innerty
	[d|
		instance (Typeable $rty,Memo $rinnerty) => Memo $rty where
			type Key $rty = Key $rinnerty
			{-# INLINE memoKey #-}
			memoKey = memoKey . $rdestrE
			{-# INLINE memoWeak #-}
			memoWeak = memoWeak . $rdestrE |]

-- | creates a @Memo@ instance not supporting memoization (i.e., all values are treated as being different)
deriveNoMemo :: Name -> Q [Dec]
deriveNoMemo name = do
	x <- reify name
	case x of
		TyConI dec -> deriveNoMemoFromDec dec
		otherwise -> error "deriveMemoId"
			
deriveNoMemoFromDec :: Dec -> Q [Dec]
deriveNoMemoFromDec dec = do
	let name = decName dec
	let decl = convert $ everywhere (mkT unKindTV) dec
	let ty = foldl AppT (ConT name) (map (VarT . mkName) $ H.dataDeclVars decl)
	let rty = return ty
	[d|
		instance Typeable $rty => Memo $rty where
			type Key $rty = Neq
			{-# INLINE memoKey #-}
			memoKey = \x -> Neq
			{-# INLINE memoWeak #-}
			memoWeak = \x -> MkWeak mkDeadWeak |]
			
decName :: Dec -> Name
decName (DataD _ name _ _ _) = name
decName (NewtypeD _ name _ _ _) = name
decName dec = error "not a data/newtype declaration"


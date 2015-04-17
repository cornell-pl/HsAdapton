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
	let decl = case x of
		TyConI dec -> convert $ everywhere (mkT unKindTV) dec
		otherwise -> error "deriveMemoId"
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
	let decl = case x of
		TyConI dec -> convert $ everywhere (mkT unKindTV) dec
		otherwise -> error "deriveMemoId"
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

-- | creates a @Memo@ instance not supporting memoization (i.e., all values are treated as being different)
deriveNoMemo :: Name -> Q [Dec]
deriveNoMemo name = do
	x <- reify name
	let decl = case x of
		TyConI dec -> convert $ everywhere (mkT unKindTV) dec
		otherwise -> error "deriveMemoId"
	let ty = foldl AppT (ConT name) (map (VarT . mkName) $ H.dataDeclVars decl)
	let rty = return ty
	[d|
		instance Typeable $rty => Memo $rty where
			type Key $rty = Neq
			{-# INLINE memoKey #-}
			memoKey = \x -> Neq
			{-# INLINE memoWeak #-}
			memoWeak = \x -> MkWeak mkDeadWeak |]
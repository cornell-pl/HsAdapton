{-# LANGUAGE OverlappingInstances, FlexibleInstances, TypeOperators, PolyKinds, ScopedTypeVariables, DeriveDataTypeable, MultiParamTypeClasses #-}

module Data.DeepTypeable where

import Data.Typeable
import Language.Haskell.TH.Syntax
import Data.Data
import qualified Data.Set as Set
import Control.Monad.State as State

import Debug.Trace

-- | A @Typeable@ class that exposes the full recursive structure of a type, not just its constructors
-- This allows testing inclusion of types more easily
class Typeable a => DeepTypeable a where
	typeTree :: Proxy a -> TypeTree
	-- the default instance treas the datatype as abstract and with no type arguments
	typeTree p = MkTypeTree (if null mod then mkName n else mkName (mod ++ "." ++ n)) [] []
		where con = typeRepTyCon $ typeRep p
		      mod = tyConModule con
		      n = tyConName con

-- we also store the type arguments of polymorphic types
data TypeTree = MkTypeTree { typeTreeName :: Name, typeTreeArgs :: [TypeTree], typeTreeCons :: [ConTree] } deriving (Typeable,Data,Show)
data ConTree = MkConTree { conTreeName :: Name, conTreeTypes :: [TypeTree] } deriving (Typeable,Data,Show) 

-- qualified type names are unique
instance Eq TypeTree where
	(MkTypeTree n1 args1 cs1) == (MkTypeTree n2 args2 cs2) = n1 == n2 && andZip args1 args2

-- qualified constructor names are unique
instance Eq ConTree where
	(MkConTree n1 tys1) == (MkConTree n2 tys2) = n1 == n2 && andZip tys1 tys2

andZip [] [] = True
andZip (x:xs) (y:ys) = x == y && andZip xs ys
andZip _ _ = False

showTypeTree :: TypeTree -> String
showTypeTree (MkTypeTree name args _) = "(" ++ foldl1 (\s1 s2 -> s1 ++' ':s2) (showName name:map showTypeTree args) ++ ")"

inDeepTypeable :: (DeepTypeable a,DeepTypeable b) => Proxy a -> Proxy b -> Bool
inDeepTypeable a b = {-trace ("inDeepTypeable: "++showTypeTree ta ++ "\n" ++ showTypeTree tb ++ "\t" ++ show check) -} check
	where check = ta `inTypeTree` tb
	      ta = typeTree a
	      tb = typeTree b

hasDeepTypeable :: (DeepTypeable a) => (TypeTree -> Bool) -> Proxy a -> Bool
hasDeepTypeable p a = hasTypeTree p (typeTree a)

-- |@inType t1 t2 = True@ iff @t1@ is (even recursively) inside @t2@
inTypeTree :: TypeTree -> TypeTree -> Bool
inTypeTree t1 t2 = hasTypeTree (==t1) t2
	
-- | Tests if a predicate holds the some recursive type inside the top-level type
hasTypeTree :: (TypeTree -> Bool) -> TypeTree -> Bool
hasTypeTree p t = State.evalState (recur t) Set.empty where
	recur t2 = do
		s <- State.gets (Set.member $ showTypeTree t2)
		if s
			then return False -- We've already seen and checked this type
			else if p t2
				then return True -- We found a matching type
				else do -- Remember that we were here
					State.modify (Set.insert $ showTypeTree t2) -- Remember that we were here
					let ctors = typeTreeCons t2 -- We need to recur on the constructors
					check (concatMap conTreeTypes ctors) -- Now we recur
	check [] = return False
	check (t:ts) = do
		t' <- recur t
		if t' then return True else check ts

instance DeepTypeable a => DeepTypeable [a] where
	typeTree p@(_::Proxy [a]) = MkTypeTree (mkName "Prelude.List") [typeTree (Proxy::Proxy a)] [MkConTree (mkName "Prelude.[]") [],MkConTree (mkName "Prelude.(:)") [typeTree (Proxy::Proxy a),typeTree p]]
{-# OPTIONS_GHC -cpp                  #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Schemes
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2003
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (local universal quantification)
--
-- \"Scrap your boilerplate\" --- Generic programming in Haskell 
-- See <http://www.cs.uu.nl/wiki/GenericProgramming/SYB>. The present module
-- provides frequently used generic traversal schemes.
--
-----------------------------------------------------------------------------

module Data.WithClass.MGenerics.Schemes (

        everywhere,
        everywhere',
        everywhereBut,
        everywhereM,
        somewhere,
        everything,
        everythingBut,
        everythingWithContext,
        listify,
        something,
        synthesize,
        gsize,
        glength,
        gdepth,
        gcount,
        gnodecount,
        gtypecount,
        gfindtype

 ) where

------------------------------------------------------------------------------

#ifdef __HADDOCK__
import Prelude
#endif
import Data.WithClass.MData
import Data.WithClass.MGenerics.Aliases
import Control.Monad
import Control.Monad.Trans.Class


-- | Apply a transformation everywhere in bottom-up manner
everywhere :: Proxy ctx -> (forall a. MData ctx m a => a -> m a)
           -> (forall a. MData ctx m a => a -> m a)

-- Use gmapT to recurse into immediate subterms;
-- recall: gmapT preserves the outermost constructor;
-- post-process recursively transformed result via f
-- 
everywhere ctx f = f <=< gmapT ctx (everywhere ctx f)


-- | Apply a transformation everywhere in top-down manner
everywhere' :: Proxy ctx -> (forall a. MData ctx m a => a -> m a)
            -> (forall a. MData ctx m a => a -> m a)

-- Arguments of (.) are flipped compared to everywhere
everywhere' ctx f = gmapT ctx (everywhere' ctx f) <=< f


-- | Variation on everywhere with an extra stop condition
everywhereBut :: Proxy ctx -> GenericQ ctx m Bool -> GenericT ctx m -> GenericT ctx m

-- Guarded to let traversal cease if predicate q holds for x
everywhereBut ctx q f x = do
	cond <- q x
	if cond
		then return x
		else f =<< (gmapT ctx (everywhereBut ctx q f) x)


-- | Monadic variation on everywhere
everywhereM :: (Monad (t m),MonadTrans t) => Proxy ctx -> GenericM ctx t m -> GenericM ctx t m

-- Bottom-up order is also reflected in order of do-actions
everywhereM ctx f x = f =<< gmapM ctx (everywhereM ctx f) x


-- | Apply a monadic transformation at least somewhere
somewhere :: (MonadPlus (t m),MonadTrans t) => Proxy ctx -> GenericM ctx t m -> GenericM ctx t m

-- We try "f" in top-down manner, but descent into "x" when we fail
-- at the root of the term. The transformation fails if "f" fails
-- everywhere, say succeeds nowhere.
-- 
somewhere ctx f x = f x `mplus` gmapMp ctx (somewhere ctx f) x


-- | Summarise all nodes in top-down, left-to-right order
everything :: Proxy ctx -> (r -> r -> m r) -> GenericQ ctx m r -> GenericQ ctx m r

-- Apply f to x to summarise top-level node;
-- use gmapQ to recurse into immediate subterms;
-- use ordinary foldl to reduce list of intermediate results
 
everything ctx k f x = gmapQ ctx (everything ctx k f) x >>= \rs -> f x >>= \r -> foldM k r rs

-- | Variation of "everything" with an added stop condition
everythingBut :: Proxy ctx -> (r -> r -> m r) -> GenericQ ctx m (r,Bool) -> GenericQ ctx m r
everythingBut ctx k f x = do
	(v, stop) <- f x
	if stop
		then return v
		else foldM k v =<< gmapQ ctx (everythingBut ctx k f) x

-- | Summarise all nodes in top-down, left-to-right order, carrying some state
-- down the tree during the computation, but not left-to-right to siblings.
everythingWithContext :: Proxy ctx -> s -> (r -> r -> m r) -> GenericQ ctx m (s -> m (r, s)) -> GenericQ ctx m r
everythingWithContext ctx s0 f q x = do
	g <- q x
	(r,s') <- g s0
	foldM f r =<< gmapQ ctx (everythingWithContext ctx s' f q) x

-- | Get a list of all entities that meet a predicate
listify :: Typeable r => Proxy ctx -> (r -> m Bool) -> GenericQ ctx m [r]
listify ctx p = everything ctx (\xs ys -> return $ xs++ys) ([] `mkQ` (\x -> p x >>= \cond -> if cond then return [x] else return []))


-- | Look up a subterm by means of a maybe-typed filter
something :: Proxy ctx -> GenericQ ctx m (Maybe u) -> GenericQ ctx m (Maybe u)

-- "something" can be defined in terms of "everything"
-- when a suitable "choice" operator is used for reduction
-- 
something ctx = everything ctx (\ma mb -> return $ orElse ma mb)


-- | Bottom-up synthesis of a data structure;
--   1st argument z is the initial element for the synthesis;
--   2nd argument o is for reduction of results from subterms;
--   3rd argument f updates the synthesised data according to the given term
--
synthesize :: Proxy ctx -> s  -> (t -> m s -> m s) -> GenericQ ctx m (s -> m t) -> GenericQ ctx m t
synthesize ctx z o f x = do
	t <- foldr o (return z) =<< gmapQ ctx (synthesize ctx z o f) x
	g <- f x
	g t

-- | Compute size of an arbitrary data structure
gsize :: MData ctx m a => Proxy ctx -> a -> m Int
gsize ctx t = liftM ((1 +) . sum) (gmapQ ctx (gsize ctx) t)


-- | Count the number of immediate subterms of the given term
glength :: Proxy ctx -> GenericQ ctx m Int
glength ctx = liftM length . gmapQ ctx (const $ return ())


-- | Determine depth of the given term
gdepth :: Proxy ctx -> GenericQ ctx m Int
gdepth ctx = liftM ((+1) . foldr max 0) . gmapQ ctx (gdepth ctx)


-- | Determine the number of all suitable nodes in a given term
gcount :: Proxy ctx -> GenericQ ctx m Bool -> GenericQ ctx m Int
gcount ctx p =  everything ctx (\x y -> return $ x+y) (\x -> p x >>= \cond -> if cond then return 1 else return 0)


-- | Determine the number of all nodes in a given term
gnodecount :: Proxy ctx -> GenericQ ctx m Int
gnodecount ctx = gcount ctx (const $ return True)


-- | Determine the number of nodes of a given type in a given term
gtypecount :: Typeable a => Proxy ctx -> a -> GenericQ ctx m Int
gtypecount ctx (_::a) = gcount ctx (False `mkQ` (\(_::a) -> return True))


-- | Find (unambiguously) an immediate subterm of a given type
gfindtype :: (MData ctx m x, Typeable y) => Proxy ctx -> x -> m (Maybe y)
gfindtype ctx = liftM singleton . foldM unJust [] <=< gmapQ ctx (Nothing `mkQ` (return . Just))
 where
  unJust l (Just x) = return $ x:l
  unJust l Nothing  = return l
  singleton [s] = Just s
  singleton _   = Nothing

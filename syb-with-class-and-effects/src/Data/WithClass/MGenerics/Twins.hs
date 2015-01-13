{-# OPTIONS_GHC -cpp                  #-}
{-# LANGUAGE ScopedTypeVariables, ImpredicativeTypes, RankNTypes               #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Twins
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2004
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (local universal quantification)
--
-- \"Scrap your boilerplate\" --- Generic programming in Haskell 
-- See <http://www.cs.uu.nl/wiki/GenericProgramming/SYB>. The present module 
-- provides support for multi-parameter traversal, which is also 
-- demonstrated with generic operations like equality.
--
-----------------------------------------------------------------------------

module Data.WithClass.MGenerics.Twins (

        -- * Generic folds and maps that also accumulate
        gfoldlAccum,
--        gmapAccumT,
--        gmapAccumM,
--        gmapAccumQl,
        gmapAccumQr,
        gmapAccumQ,
--        gmapAccumA,

        -- * Mapping combinators for twin traversal
--        gzipWithT,
--        gzipWithM,
        gzipWithQ,

        -- * Typical twin traversals
        geq,
--        gzip

  ) where


------------------------------------------------------------------------------

import Control.Monad

#ifdef __HADDOCK__
import Prelude
#endif
import Data.WithClass.MData
import Data.WithClass.MGenerics.Aliases

#ifdef __GLASGOW_HASKELL__
import Prelude hiding ( GT )
#endif

import Control.Applicative (Applicative(..))

------------------------------------------------------------------------------


------------------------------------------------------------------------------
--
--      Generic folds and maps that also accumulate
--
------------------------------------------------------------------------------

{--------------------------------------------------------------

A list map can be elaborated to perform accumulation.
In the same sense, we can elaborate generic maps over terms.

We recall the type of map:
map :: (a -> b) -> [a] -> [b]

We recall the type of an accumulating map (see Data.List):
mapAccumL :: (a -> b -> (a,c)) -> a -> [b] -> (a,[c])

Applying the same scheme we obtain an accumulating gfoldl.

--------------------------------------------------------------}

-- | gfoldl with accumulation

gfoldlAccum :: MData ctx m d
            => Proxy ctx -> (forall e r. MData ctx m e => a -> c (m e -> m r) -> m e -> m (a, c r))
            -> (forall g. a -> g -> m (a, c g))
            -> a -> d -> m (a, c d)

gfoldlAccum ctx k z a0 d = do { a <- gfoldl ctx k' z' d; unA a a0 }
	where
	k' c y = return $ A $ \a -> do { (a', c') <- unA c a; k a' c' y }
	z' f   = return $ A $ \a -> z a f

-- | A type constructor for accumulation
newtype A a c m d = A { unA :: a -> m (a, c d) }


---- | gmapT with accumulation
--gmapAccumT :: Data d
--           => (forall e. Data e => a -> e -> (a,e))
--           -> a -> d -> (a, d)
--gmapAccumT f a0 d0 = let (a1, d1) = gfoldlAccum k z a0 d0
--                     in (a1, unID d1)
-- where
--  k a (ID c) d = let (a',d') = f a d
--                  in (a', ID (c d'))
--  z a x = (a, ID x)
--
--
---- | Applicative version
--gmapAccumA :: forall b d a. (Data d, Applicative a)
--           => (forall e. Data e => b -> e -> (b, a e))
--           -> b -> d -> (b, a d)
--gmapAccumA f a0 d0 = gfoldlAccum k z a0 d0
--    where
--      k :: forall d' e. (Data d') =>
--           b -> a (d' -> e) -> d' -> (b, a e)
--      k a c d = let (a',d') = f a d
--                    c' = c <*> d'
--                in (a', c')
--      z :: forall t c a'. (Applicative a') =>
--           t -> c -> (t, a' c)
--      z a x = (a, pure x)
--
--
---- | gmapM with accumulation
--gmapAccumM :: (Data d, Monad m)
--           => (forall e. Data e => a -> e -> (a, m e))
--           -> a -> d -> (a, m d)
--gmapAccumM f = gfoldlAccum k z
-- where
--  k a c d = let (a',d') = f a d
--             in (a', d' >>= \d'' -> c >>= \c' -> return (c' d''))
--  z a x = (a, return x)
--
--
---- | gmapQl with accumulation
--gmapAccumQl :: Data d
--            => (r -> r' -> r)
--            -> r
--            -> (forall e. Data e => a -> e -> (a,r'))
--            -> a -> d -> (a, r)
--gmapAccumQl o r0 f a0 d0 = let (a1, r1) = gfoldlAccum k z a0 d0
--                           in (a1, unCONST r1)
-- where
--  k a (CONST c) d = let (a', r) = f a d
--                     in (a', CONST (c `o` r))
--  z a _ = (a, CONST r0)

-- | gmapQr with accumulation
gmapAccumQr :: MData ctx m d
            => Proxy ctx -> (r' -> r -> m r)
            -> r
            -> (forall e. MData ctx m e => a -> e -> m (a,r'))
            -> a -> d -> m (a,r)
gmapAccumQr ctx o r0 f a0 d0 = do
		(a1,l) <- gfoldlAccum ctx k z a0 d0
		r1 <- unQr l r0
		return (a1,r1)
	where
	k a (Qr c) d = do
		(a',r') <- f a =<< d
		return (a', Qr (\r -> c =<< (r' `o` r)))
	z a _ = return (a, Qr return)

-- | gmapQ with accumulation
gmapAccumQ :: MData ctx m d
           => Proxy ctx -> (forall e. MData ctx m e => a -> e -> m (a,q))
           -> a -> d -> m (a, [q])
gmapAccumQ ctx f = gmapAccumQr ctx (\x xs -> return $ x:xs) [] f



--------------------------------------------------------------------------------
----
----      Helper type constructors
----
--------------------------------------------------------------------------------
--
--
---- | The identity type constructor needed for the definition of gmapAccumT
--newtype ID x = ID { unID :: x }
--
--
---- | The constant type constructor needed for the definition of gmapAccumQl
--newtype CONST c a = CONST { unCONST :: c }


-- | The type constructor needed for the definition of gmapAccumQr
newtype Qr r m a = Qr { unQr  :: r -> m r }



--------------------------------------------------------------------------------
----
----      Mapping combinators for twin traversal
----
--------------------------------------------------------------------------------
--
--
---- | Twin map for transformation 
--gzipWithT :: GenericQ (GenericT) -> GenericQ (GenericT)
--gzipWithT f x y = case gmapAccumT perkid funs y of
--                    ([], c) -> c
--                    _       -> error "gzipWithT"
-- where
--  perkid a d = (tail a, unGT (head a) d)
--  funs = gmapQ (\k -> GT (f k)) x
--
--
--
---- | Twin map for monadic transformation 
--gzipWithM :: Monad m => GenericQ (GenericM m) -> GenericQ (GenericM m)
--gzipWithM f x y = case gmapAccumM perkid funs y of
--                    ([], c) -> c
--                    _       -> error "gzipWithM"
-- where
--  perkid a d = (tail a, unGM (head a) d)
--  funs = gmapQ (\k -> GM (f k)) x
--

-- | Twin map for queries
gzipWithQ :: Proxy ctx -> GenericQ ctx m (GenericQ' ctx m r) -> GenericQ ctx m (GenericQ' ctx m [r])
gzipWithQ ctx f = aux1 ctx f
	where
	perkid :: MData ctx m e => Proxy ctx -> [GenericQ' ctx m r] -> e -> m ([GenericQ' ctx m r],r)
	perkid ctx a d = unGQ (head a) d >>= \r -> return (tail a,r)
	
	aux1 :: Proxy ctx -> GenericQ ctx m (GenericQ' ctx m r) -> GenericQ ctx m (GenericQ' ctx m [r])
	aux1 ctx f x = do
		funs <- gmapQ ctx f x
		return $ aux2 ctx funs
	
	aux2 :: Proxy ctx -> [GenericQ' ctx m r] -> GenericQ' ctx m [r]
	aux2 ctx funs = GQ $ \y -> do
		res <- gmapAccumQ ctx (perkid ctx) funs y
		case res of
			([], r) -> return r
			_       -> fail "gzipWithQ"

------------------------------------------------------------------------------
--
--      Typical twin traversals
--
------------------------------------------------------------------------------

-- | Generic equality: an alternative to \"deriving Eq\"
geq :: MData ctx m a => Proxy ctx -> a -> a -> m Bool

{-

Testing for equality of two terms goes like this. Firstly, we
establish the equality of the two top-level datatype
constructors. Secondly, we use a twin gmap combinator, namely tgmapQ,
to compare the two lists of immediate subterms.

(Note for the experts: the type of the worker geq' is rather general
but precision is recovered via the restrictive type of the top-level
operation geq. The imprecision of geq' is caused by the type system's
unability to express the type equivalence for the corresponding
couples of immediate subterms from the two given input terms.)

-}

geq ctx x0 y0 = do { GQ q <- geq' ctx x0; q y0 }
	where
	geq' :: Proxy ctx -> GenericQ ctx m (GenericQ' ctx m Bool)
	geq' ctx x = do
		xConstr <- toConstr ctx x
		return $ GQ $ \y -> do
			yConstr <- toConstr ctx y
			GQ q <- gzipWithQ ctx (geq' ctx) x
			bs <- q y
			return $ (xConstr == yConstr) && (and bs)

---- | Generic zip controlled by a function with type-specific branches
--gzip :: GenericQ (GenericM Maybe) -> GenericQ (GenericM Maybe)
---- See testsuite/.../Generics/gzip.hs for an illustration
--gzip f x y =
--  f x y
--  `orElse`
--  if toConstr x == toConstr y
--    then gzipWithM (gzip f) x y
--    else Nothing

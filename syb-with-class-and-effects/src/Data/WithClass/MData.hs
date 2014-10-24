{-# LANGUAGE UndecidableInstances, ImpredicativeTypes, FlexibleContexts,MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, PolyKinds #-}
{-# LANGUAGE TemplateHaskell, StandaloneDeriving, DeriveDataTypeable, TypeOperators,
             GADTs #-}

module Data.WithClass.MData (
	  module Data.Typeable
	, module Data.DeepTypeable
	, module Data.Generics.SYB.WithClass.Context
	, proxyNoCtx
	, MData
	, gfoldl
	, gunfold
	, toConstr
	, dataTypeOf
	, dataCast1
	, dataCast2
	, gmapT
	, gmapQl
	, gmapQr
	, gmapQ
	, gmapQi
	, gmapM
	, gmapMp
	, gmapMo
	, fromConstr
	, fromConstrB
	, fromConstrM
	, Constr(..)
	, DataType(..)
	, constrIndex
	, mkConstr
	, mkDataType
	, Fixity(..)
	, ConstrRep(..)
	, constrRep
	, indexConstr
	, mkNoRepType
	, mkIntegralConstr
	, mkIntType
	, mkRealConstr
	, mkFloatType
	, mkCharType
	, mkCharConstr
	, showConstr
	, dataTypeConstrs
	, proxyOf,typeRepOf
	) where


------------------------------------------------------------------------------


import Prelude -- necessary to get dependencies right

import Data.DeriveTH                 -- Library for deriving instances for existing types
import Data.DeepTypeable
import Data.WithClass.Derive.DeepTypeable
import Language.Haskell.TH.Syntax hiding (lift,Infix,Fixity)

import Data.Data (Data,Constr(..),DataType(..),showConstr,constrIndex,dataTypeConstrs,mkConstr,mkDataType,Fixity(..),ConstrRep(..),constrRep,indexConstr,mkNoRepType,mkIntegralConstr,mkIntType,mkRealConstr,mkFloatType,mkCharConstr,mkCharType)
import qualified Data.Data as Data
import Data.Typeable
import Data.Maybe
import Data.Version( Version(..) )
import Control.Monad
import Control.Monad.Trans.Class
import Data.DeepTypeable
import Language.Haskell.Exts as H hiding (Fixity)

-- Imports for the instances
import Data.Int              -- So we can give Data instance for Int8, ...
import Data.Type.Coercion
import Data.Word             -- So we can give Data instance for Word8, ...
import GHC.Real( Ratio(..) ) -- So we can give Data instance for Ratio
--import GHC.IOBase            -- So we can give Data instance for IO, Handle
import GHC.Prim
import GHC.Ptr               -- So we can give Data instance for Ptr
import GHC.ForeignPtr        -- So we can give Data instance for ForeignPtr
--import GHC.Stable            -- So we can give Data instance for StablePtr
--import GHC.ST                -- So we can give Data instance for ST
--import GHC.Conc              -- So we can give Data instance for MVar & Co.
import GHC.Arr               -- So we can give Data instance for Array
import Data.IORef

import Data.Generics.SYB.WithClass.Context

import Data.Typeable.Internal

------------------------------------------------------------------------------
--
--      The MData class
--
------------------------------------------------------------------------------

-- | Converts a value of type @a@ into a @Proxy a@
proxyOf :: a -> Proxy a
proxyOf _ = Proxy

-- | Gets the type representation of (the type of) a value
typeRepOf :: Typeable a => a -> TypeRep
typeRepOf a = typeRep (proxyOf a)


proxyNoCtx :: Proxy NoCtx
proxyNoCtx = Proxy

class (Monad m,DeepTypeable a,Sat (ctx a)) => MData ctx m a where

  gfoldl  :: Proxy ctx -> (forall d b. MData ctx m d => c (m d -> m b) -> m d -> m (c b))
          -> (forall g. g -> m (c g))
          -> a
          -> m (c a)
  gfoldl ctx _ z = z

  gunfold :: Proxy ctx -> (forall b r. MData ctx m b => c (m b -> m r) -> m (c r)) -> (forall r. r -> m (c r)) -> Constr -> m (c a)

  toConstr :: Proxy ctx -> a -> m Constr

  dataTypeOf :: Proxy ctx -> a -> m DataType

------------------------------------------------------------------------------
--
-- Mediate types and type constructors
--
------------------------------------------------------------------------------

  -- | Mediate types and unary type constructors.
  -- In 'Data' instances of the form @T a@, 'dataCast1' should be defined
  -- as 'gcast1'.
  --
  -- The default definition is @'const' 'Nothing'@, which is appropriate
  -- for non-unary type constructors.
  dataCast1 :: Typeable t
            => Proxy ctx -> (forall d. MData ctx m d => m (c (t d)))
            -> m (Maybe (c a))
  dataCast1 ctx _ = return Nothing

  -- | Mediate types and binary type constructors.
  -- In 'Data' instances of the form @T a b@, 'dataCast2' should be
  -- defined as 'gcast2'.
  --
  -- The default definition is @'const' 'Nothing'@, which is appropriate
  -- for non-binary type constructors.
  dataCast2 :: Typeable t
            => Proxy ctx -> (forall d e. (MData ctx m d, MData ctx m e) => m (c (t d e)))
            -> m (Maybe (c a))
  dataCast2 ctx _ = return Nothing

------------------------------------------------------------------------------
--
--      Typical generic maps defined in terms of gfoldl
--
------------------------------------------------------------------------------


  -- | A generic transformation that maps over the immediate subterms
  --
  -- The default definition instantiates the type constructor @c@ in the
  -- type of 'gfoldl' to an identity datatype constructor, using the
  -- isomorphism pair as injection and projection.
  gmapT :: Proxy ctx -> (forall b. MData ctx m b => b -> m b) -> a -> m a

  -- Use an identity datatype constructor ID (see below)
  -- to instantiate the type constructor c in the type of gfoldl,
  -- and perform injections ID and projections unID accordingly.
  --
  gmapT ctx f x0 = liftM unID (gfoldl ctx k (return . ID) x0)
    where
      k :: MData ctx m d => ID (m d -> m b) -> m d -> m (ID b)
      k (ID c) mx = do { b1 <- c (mx >>= f); return (ID b1) }


  -- | A generic query with a left-associative binary operator
  gmapQl :: forall r r'. Proxy ctx -> (r -> r' -> m r) -> r -> (forall d. MData ctx m d => d -> m r') -> a -> m r
  gmapQl ctx o r f = liftM unCONST . gfoldl ctx k z
    where
      k :: MData ctx m d => CONST r (m d -> m b) -> m d -> m (CONST r b)
      k c mx = do { r2 <- mx >>= f; r3 <- unCONST c `o` r2; return $ CONST r3 }
      z :: g -> m (CONST r g)
      z _   = return $ CONST r

  -- | A generic query with a right-associative binary operator
  gmapQr :: forall r r'. Proxy ctx -> (r' -> r -> m r) -> r -> (forall d. MData ctx m d => d -> m r') -> a -> m r
  gmapQr ctx o r0 f x0 = do { q <- gfoldl ctx (k ctx) z x0; unQr q r0 }
    where
      k :: MData ctx m d => Proxy ctx -> Qr m r (m d -> m b) -> m d -> m (Qr m r b)
      k ctx (Qr c) mx = return $ Qr $ \r -> do { r2 <- mx >>= f; r3 <- r2 `o` r; c r3 }
      z :: g -> m (Qr m r g)
      z _ = return $ Qr return


  -- | A generic query that processes the immediate subterms and returns a list
  -- of results.  The list is given in the same order as originally specified
  -- in the declaratoin of the data constructors.
  gmapQ :: Proxy ctx -> (forall d. MData ctx m d => d -> m u) -> a -> m [u]
  gmapQ ctx f = gmapQr ctx (\x xs -> return $ x:xs) [] f


  -- | A generic query that processes one child by index (zero-based)
  gmapQi :: forall u. Proxy ctx -> Int -> (forall d. MData ctx m d => d -> m u) -> a -> m u
  gmapQi ctx i f x = gfoldl ctx (k ctx) z x >>= \res -> case res of { Qi _ q -> fromJust q }
    where
      k :: MData ctx m d => Proxy ctx -> Qi m u (m d -> m b) -> m d -> m (Qi m u b)
      k ctx (Qi i' q) ma = return $ Qi (i'+1) (if i==i' then Just (ma >>= f) else q)
      z :: g -> m (Qi m q g)
      z _           = return $ Qi 0 Nothing


  -- | A generic monadic transformation that maps over the immediate subterms
  --
  -- The default definition instantiates the type constructor @c@ in
  -- the type of 'gfoldl' to the monad datatype constructor, defining
  -- injection and projection using 'return' and '>>='.
  gmapM :: forall t. (Monad (t m),MonadTrans t) => Proxy ctx -> (forall d. MData ctx m d => d -> t m d) -> a -> t m a

  -- Use immediately the monad datatype constructor 
  -- to instantiate the type constructor c in the type of gfoldl,
  -- so injection and projection is done by return and >>=.
  --  
  gmapM ctx f = join . lift . gfoldl ctx k z where
	k :: MData ctx m d => t m (m d -> m b) -> m d -> m (t m b)
	k c mx = return $ do
		c' <- c
		x' <- lift mx >>= f
		lift $ c' $ return x'
	z :: g -> m (t m g)
	z g = return $ return g

  -- | Transformation of at least one immediate subterm does not fail
  gmapMp :: forall t. (MonadPlus (t m),MonadTrans t) => Proxy ctx -> (forall d. MData ctx m d => d -> t m d) -> a -> t m a

{-

The type constructor that we use here simply keeps track of the fact
if we already succeeded for an immediate subterm; see Mp below. To
this end, we couple the monadic computation with a Boolean.

-}

  gmapMp ctx f x = lift (gfoldl ctx k z x) >>= unMp >>= \(x',b) ->
                if b then return x' else mzero
    where
      z :: g -> m (Mp (t m) g)
      z g = return $ Mp $ return (g,False)
      k :: MData ctx m d => Mp (t m) (m d -> m b) -> m d -> m (Mp (t m) b)
      k (Mp c) my
        = return $ Mp ( c >>= \(h, b) ->
                 (lift my >>= f >>= \y' -> lift (h $ return y') >>= \x' -> return (x', True))
                 `mplus` (lift (h my) >>= \x -> return (x, b)))

  -- | Transformation of one immediate subterm with success
  gmapMo :: forall t. (MonadPlus (t m),MonadTrans t) => Proxy ctx -> (forall d. MData ctx m d => d -> t m d) -> a -> t m a

{-

We use the same pairing trick as for gmapMp, 
i.e., we use an extra Bool component to keep track of the 
fact whether an immediate subterm was processed successfully.
However, we cut of mapping over subterms once a first subterm
was transformed successfully.

-}

  gmapMo ctx f x = lift (gfoldl ctx k z x) >>= unMp >>= \(x',b) ->
                if b then return x' else mzero
    where
      z :: g -> m (Mp (t m) g)
      z g = return $ Mp $ return (g,False)
      k :: MData ctx m d => Mp (t m) (m d -> m b) -> m d -> m (Mp (t m) b)
      k (Mp c) my
        = return $ Mp ( c >>= \(h,b) -> if b
                        then lift (h my) >>= \x -> return (x, b)
                        else (lift my >>= f >>= \y' -> lift (h $ return y') >>= \x' -> return (x',True))
                             `mplus` (lift (h my) >>= \x -> return (x, b))
             )


-- | The identity type constructor needed for the definition of gmapT
newtype ID x = ID { unID :: x }


-- | The constant type constructor needed for the definition of gmapQl
newtype CONST c a = CONST { unCONST :: c }


-- | Type constructor for adding counters to queries
data Qi m q a = Qi Int (Maybe (m q))


-- | The type constructor used in definition of gmapQr
newtype Qr m r a = Qr { unQr  :: r -> m r }


-- | The type constructor used in definition of gmapMp
newtype Mp m x = Mp { unMp :: m (x, Bool) }

------------------------------------------------------------------------------
--
--      Generic unfolding
--
------------------------------------------------------------------------------


-- | Build a term skeleton
fromConstr :: MData ctx m a => Proxy ctx -> Constr -> m a
fromConstr ctx = fromConstrB ctx (error "Data.MData.fromConstr")


-- | Build a term and use a generic function for subterms
fromConstrB :: MData ctx m a
            => Proxy ctx -> (forall d. MData ctx m d => m d)
            -> Constr
            -> m a
fromConstrB ctx f = liftM unID . gunfold ctx (k ctx f) z where
	k :: MData ctx m b => Proxy ctx -> (forall d. MData ctx m d => m d) -> ID (m b -> m r) -> m (ID r)
	k ctx f c = unID c f >>= return . ID
	
	z :: Monad m => r -> m (ID r)
	z = return . ID

-- | Monadic variation on 'fromConstrB'
fromConstrM :: (Monad (t m), MonadTrans t,MData ctx m a)
            => Proxy ctx -> (forall d. MData ctx m d => t m d)
            -> Constr
            -> t m a
fromConstrM ctx f = join . lift . gunfold ctx (k ctx f) z where
	k :: (MData ctx m b,Monad (t m),MonadTrans t) => Proxy ctx -> (forall d. MData ctx m d => t m d) -> t m (m b -> m r) -> m (t m r)
	k ctx f c = return $ do
		c' <- c
		b <- f
		lift (c' $ return b)
	
	z :: (Monad m,Monad (t m)) => r -> m (t m r)
	z = return . return

------------------------------------------------------------------------------
------------------------------------------------------------------------------
--
--      Instances of the Data class for Prelude-like types.
--      We define top-level definitions for representations.
--
------------------------------------------------------------------------------


falseConstr :: Constr
falseConstr  = mkConstr boolDataType "False" [] Prefix
trueConstr :: Constr
trueConstr   = mkConstr boolDataType "True"  [] Prefix

boolDataType :: DataType
boolDataType = mkDataType "Prelude.Bool" [falseConstr,trueConstr]

instance (Monad m,Sat (ctx Bool)) => MData ctx m Bool where
  toConstr ctx False = return falseConstr
  toConstr ctx True  = return trueConstr
  gunfold ctx _ z c  = case constrIndex c of
                     1 -> z False
                     2 -> z True
                     _ -> error $ "Data.MData.gunfold: Constructor "
                                  ++ show c
                                  ++ " is not of type Bool."
  dataTypeOf ctx _ = return boolDataType

$( derive makeDeepTypeableAbstract ''Bool )

----------------------------------------------------------------------------

charType :: DataType
charType = mkCharType "Prelude.Char"

instance (Monad m,Sat (ctx Char)) => MData ctx m Char where
  toConstr ctx x = return $ mkCharConstr charType x
  gunfold ctx _ z c = case constrRep c of
                    (CharConstr x) -> z x
                    _ -> error $ "Data.MData.gunfold: Constructor " ++ show c
                                 ++ " is not of type Char."
  dataTypeOf ctx _ = return charType

$( derive makeDeepTypeableAbstract ''Char )

------------------------------------------------------------------------------

floatType :: DataType
floatType = mkFloatType "Prelude.Float"

instance (Monad m,Sat (ctx Float)) => MData ctx m Float where
  toConstr ctx = return . mkRealConstr floatType
  gunfold ctx _ z c = case constrRep c of
                    (FloatConstr x) -> z (realToFrac x)
                    _ -> error $ "Data.MData.gunfold: Constructor " ++ show c
                                 ++ " is not of type Float."
  dataTypeOf ctx _ = return floatType

$( derive makeDeepTypeableAbstract ''Float )

------------------------------------------------------------------------------

doubleType :: DataType
doubleType = mkFloatType "Prelude.Double"

instance (Monad m,Sat (ctx Double)) => MData ctx m Double where
  toConstr ctx = return . mkRealConstr doubleType
  gunfold ctx _ z c = case constrRep c of
                    (FloatConstr x) -> z (realToFrac x)
                    _ -> error $ "Data.MData.gunfold: Constructor " ++ show c
                                 ++ " is not of type Double."
  dataTypeOf ctx _ = return doubleType

$( derive makeDeepTypeableAbstract ''Double )

------------------------------------------------------------------------------

intType :: DataType
intType = mkIntType "Prelude.Int"

instance (Monad m,Sat (ctx Int)) => MData ctx m Int where
  toConstr ctx x = return $ mkIntegralConstr intType x
  gunfold ctx _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.MData.gunfold: Constructor " ++ show c
                                 ++ " is not of type Int."
  dataTypeOf ctx _ = return intType

$( derive makeDeepTypeableAbstract ''Int )

------------------------------------------------------------------------------

integerType :: DataType
integerType = mkIntType "Prelude.Integer"

instance (Monad m,Sat (ctx Integer)) => MData ctx m Integer where
  toConstr ctx = return . mkIntegralConstr integerType
  gunfold ctx _ z c = case constrRep c of
                    (IntConstr x) -> z x
                    _ -> error $ "Data.MData.gunfold: Constructor " ++ show c
                                 ++ " is not of type Integer."
  dataTypeOf ctx _ = return integerType

$( derive makeDeepTypeableAbstract ''Integer )

------------------------------------------------------------------------------

int8Type :: DataType
int8Type = mkIntType "Data.Int.Int8"

instance (Monad m,Sat (ctx Int8)) => MData ctx m Int8 where
  toConstr ctx x = return $ mkIntegralConstr int8Type x
  gunfold ctx _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.MData.gunfold: Constructor " ++ show c
                                 ++ " is not of type Int8."
  dataTypeOf ctx _ = return int8Type

$( derive makeDeepTypeableAbstract ''Int8 )

------------------------------------------------------------------------------

int16Type :: DataType
int16Type = mkIntType "Data.Int.Int16"

instance (Monad m,Sat (ctx Int16)) => MData ctx m Int16 where
  toConstr ctx x = return $ mkIntegralConstr int16Type x
  gunfold ctx _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.MData.gunfold: Constructor " ++ show c
                                 ++ " is not of type Int16."
  dataTypeOf ctx _ = return int16Type

$( derive makeDeepTypeableAbstract ''Int16 )

------------------------------------------------------------------------------

int32Type :: DataType
int32Type = mkIntType "Data.Int.Int32"

instance (Monad m,Sat (ctx Int32)) => MData ctx m Int32 where
  toConstr ctx x = return $ mkIntegralConstr int32Type x
  gunfold ctx _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.MData.gunfold: Constructor " ++ show c
                                 ++ " is not of type Int32."
  dataTypeOf ctx _ = return int32Type

$( derive makeDeepTypeableAbstract ''Int32 )

------------------------------------------------------------------------------

int64Type :: DataType
int64Type = mkIntType "Data.Int.Int64"

instance (Monad m,Sat (ctx Int64)) => MData ctx m Int64 where
  toConstr ctx x = return $ mkIntegralConstr int64Type x
  gunfold ctx _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.MData.gunfold: Constructor " ++ show c
                                 ++ " is not of type Int64."
  dataTypeOf ctx _ = return int64Type

$( derive makeDeepTypeableAbstract ''Int64 )

------------------------------------------------------------------------------

wordType :: DataType
wordType = mkIntType "Data.Word.Word"

instance (Monad m,Sat (ctx Word)) => MData ctx m Word where
  toConstr ctx x = return $ mkIntegralConstr wordType x
  gunfold ctx _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.MData.gunfold: Constructor " ++ show c
                                 ++ " is not of type Word"
  dataTypeOf ctx _ = return wordType

$( derive makeDeepTypeableAbstract ''Word )

------------------------------------------------------------------------------

word8Type :: DataType
word8Type = mkIntType "Data.Word.Word8"

instance (Monad m,Sat (ctx Word8)) => MData ctx m Word8 where
  toConstr ctx x = return $ mkIntegralConstr word8Type x
  gunfold ctx _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.MData.gunfold: Constructor " ++ show c
                                 ++ " is not of type Word8."
  dataTypeOf ctx _ = return word8Type

$( derive makeDeepTypeableAbstract ''Word8 )

------------------------------------------------------------------------------

word16Type :: DataType
word16Type = mkIntType "Data.Word.Word16"

instance (Monad m,Sat (ctx Word16)) => MData ctx m Word16 where
  toConstr ctx x = return $ mkIntegralConstr word16Type x
  gunfold ctx _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.MData.gunfold: Constructor " ++ show c
                                 ++ " is not of type Word16."
  dataTypeOf ctx _ = return word16Type

$( derive makeDeepTypeableAbstract ''Word16 )

------------------------------------------------------------------------------

word32Type :: DataType
word32Type = mkIntType "Data.Word.Word32"

instance (Monad m,Sat (ctx Word32)) => MData ctx m Word32 where
  toConstr ctx x = return $ mkIntegralConstr word32Type x
  gunfold ctx _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.MData.gunfold: Constructor " ++ show c
                                 ++ " is not of type Word32."
  dataTypeOf ctx _ = return word32Type

$( derive makeDeepTypeableAbstract ''Word32 )

------------------------------------------------------------------------------

word64Type :: DataType
word64Type = mkIntType "Data.Word.Word64"

instance (Monad m,Sat (ctx Word64)) => MData ctx m Word64 where
  toConstr ctx x = return $ mkIntegralConstr word64Type x
  gunfold ctx _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.MData.gunfold: Constructor " ++ show c
                                 ++ " is not of type Word64."
  dataTypeOf ctx _ = return word64Type

$( derive makeDeepTypeableAbstract ''Word64 )

------------------------------------------------------------------------------

ratioConstr :: Constr
ratioConstr = mkConstr ratioDataType ":%" [] Infix

ratioDataType :: DataType
ratioDataType = mkDataType "GHC.Real.Ratio" [ratioConstr]

instance (MData ctx m a, Integral a,Sat (ctx (Ratio a))) => MData ctx m (Ratio a) where
  gfoldl ctx k z (a :% b) = z (\mx -> return $ \my -> mx >>= \x -> my >>= \y -> return $ x:%y) >>= flip k (return a) >>= flip k (return b)
  toConstr ctx _ = return ratioConstr
  gunfold ctx k z c | constrIndex c == 1 = z (\mx -> return $ \my -> mx >>= \x -> my >>= \y -> return $ x:%y) >>= k >>= k
  gunfold ctx _ _ _ = error "Data.MData.gunfold(Ratio)"
  dataTypeOf ctx _  = return ratioDataType

instance (DeepTypeable a) => DeepTypeable (Ratio a) where
	typeTree (_::Proxy (Ratio a)) = MkTypeTree (mkName "Data.Ratio.Ratio") [typeTree (Proxy :: Proxy a)] [MkConTree (mkName "Data.Ratio.Ratio") [typeTree (Proxy :: Proxy a)]]


----------------------------------------------------------------------------

nilConstr :: Constr
nilConstr    = mkConstr listDataType "[]" [] Prefix
consConstr :: Constr
consConstr   = mkConstr listDataType "(:)" [] Infix

listDataType :: DataType
listDataType = mkDataType "Prelude.[]" [nilConstr,consConstr]

instance (MData ctx m a,Sat (ctx [a])) => MData ctx m [a] where
  gfoldl ctx _ z []     = z []
  gfoldl ctx f z (x:xs) = z (\mx -> return $ \my -> mx >>= \x -> my >>= \y -> return $ x:y) >>= flip f (return x) >>= flip f (return xs)
  toConstr ctx []    = return nilConstr
  toConstr ctx (_:_) = return consConstr
  gunfold ctx k z c = case constrIndex c of
                    1 -> z []
                    2 -> z (\mx -> return $ \my -> mx >>= \x -> my >>= \y -> return $ x:y) >>= k >>= k
                    _ -> error "Data.MData.gunfold(List)"
  dataTypeOf ctx _ = return $ listDataType
  dataCast1 ctx f  = liftM gcast1 f

--
-- The gmaps are given as an illustration.
-- This shows that the gmaps for lists are different from list maps.
--
  gmapT  ctx _   []     = return []
  gmapT  ctx f   (x:xs) = do { y <- f x ; ys <- f xs ; return (y:ys) }
  gmapQ  ctx _   []     = return []
  gmapQ  ctx f   (x:xs) = do { y <- f x ; ys <- f xs ; return [y,ys] }
  gmapM  ctx _   []     = return []
  gmapM  ctx f   (x:xs) = f x >>= \x' -> f xs >>= \xs' -> return (x':xs')

------------------------------------------------------------------------------

nothingConstr :: Constr
nothingConstr = mkConstr maybeDataType "Nothing" [] Prefix
justConstr :: Constr
justConstr    = mkConstr maybeDataType "Just"    [] Prefix

maybeDataType :: DataType
maybeDataType = mkDataType "Prelude.Maybe" [nothingConstr,justConstr]

instance (MData ctx m a,Sat (ctx (Maybe a))) => MData ctx m (Maybe a) where
  gfoldl ctx _ z Nothing  = z Nothing
  gfoldl ctx f z (Just x) = z (liftM Just) >>= flip f (return x)
  toConstr ctx Nothing  = return nothingConstr
  toConstr ctx (Just _) = return justConstr
  gunfold ctx k z c = case constrIndex c of
                    1 -> z Nothing
                    2 -> z (liftM Just) >>= k
                    _ -> error "Data.MData.gunfold(Maybe)"
  dataTypeOf ctx _ = return $ maybeDataType
  dataCast1 ctx f  = liftM gcast1 f


instance (DeepTypeable a) => DeepTypeable (Maybe a) where
	typeTree (_::Proxy (Maybe a)) = MkTypeTree (mkName "Prelude.Maybe") [typeTree (Proxy :: Proxy a)] [MkConTree (mkName "Prelude.Nothing") [],MkConTree (mkName "Prelude.Just") [typeTree (Proxy :: Proxy a)]]

------------------------------------------------------------------------------

ltConstr :: Constr
ltConstr         = mkConstr orderingDataType "LT" [] Prefix
eqConstr :: Constr
eqConstr         = mkConstr orderingDataType "EQ" [] Prefix
gtConstr :: Constr
gtConstr         = mkConstr orderingDataType "GT" [] Prefix

orderingDataType :: DataType
orderingDataType = mkDataType "Prelude.Ordering" [ltConstr,eqConstr,gtConstr]

instance (Monad m,Sat (ctx Ordering)) => MData ctx m Ordering where
  gfoldl ctx _ z LT  = z LT
  gfoldl ctx _ z EQ  = z EQ
  gfoldl ctx _ z GT  = z GT
  toConstr ctx LT  = return ltConstr
  toConstr ctx EQ  = return eqConstr
  toConstr ctx GT  = return gtConstr
  gunfold ctx _ z c = case constrIndex c of
                    1 -> z LT
                    2 -> z EQ
                    3 -> z GT
                    _ -> error "Data.MData.gunfold(Ordering)"
  dataTypeOf ctx _ = return $ orderingDataType

$( derive makeDeepTypeable ''Ordering )

------------------------------------------------------------------------------

leftConstr :: Constr
leftConstr     = mkConstr eitherDataType "Left"  [] Prefix

rightConstr :: Constr
rightConstr    = mkConstr eitherDataType "Right" [] Prefix

eitherDataType :: DataType
eitherDataType = mkDataType "Prelude.Either" [leftConstr,rightConstr]

instance (MData ctx m a, MData ctx m b,Sat (ctx (Either a b))) => MData ctx m (Either a b) where
  gfoldl ctx f z (Left a)   = z (liftM Left)  >>= flip f (return a)
  gfoldl ctx f z (Right a)  = z (liftM Right) >>= flip f (return a)
  toConstr ctx (Left _)  = return leftConstr
  toConstr ctx (Right _) = return rightConstr
  gunfold ctx k z c = case constrIndex c of
                    1 -> z (liftM Left) >>= k
                    2 -> z (liftM Right) >>= k
                    _ -> error "Data.MData.gunfold(Either)"
  dataTypeOf ctx _ = return eitherDataType
  dataCast2 ctx f  = liftM gcast2 f

instance (DeepTypeable a,DeepTypeable b) => DeepTypeable (Either a b) where
	typeTree (_::Proxy (Either a b)) = MkTypeTree (mkName "Prelude.Either") [typeTree (Proxy :: Proxy a),typeTree (Proxy :: Proxy b)] [MkConTree (mkName "Prelude.Left") [typeTree (Proxy :: Proxy a)],MkConTree (mkName "Prelude.Right") [typeTree (Proxy :: Proxy b)]]

------------------------------------------------------------------------------

tuple0Constr :: Constr
tuple0Constr = mkConstr tuple0DataType "()" [] Prefix

tuple0DataType :: DataType
tuple0DataType = mkDataType "Prelude.()" [tuple0Constr]

instance (Monad m,Sat (ctx ())) => MData ctx m () where
  toConstr ctx ()   = return tuple0Constr
  gunfold ctx _ z c | constrIndex c == 1 = z ()
  gunfold ctx _ _ _ = error "Data.MData.gunfold(unit)"
  dataTypeOf ctx _  = return tuple0DataType

instance DeepTypeable ()

------------------------------------------------------------------------------

tuple2Constr :: Constr
tuple2Constr = mkConstr tuple2DataType "(,)" [] Infix

tuple2DataType :: DataType
tuple2DataType = mkDataType "Prelude.(,)" [tuple2Constr]

instance (MData ctx m a, MData ctx m b,Sat (ctx (a,b))) => MData ctx m (a,b) where
  gfoldl ctx f z (a,b) = z (\mx1 -> return $ \mx2 -> mx1 >>= \x1 -> mx2 >>= \x2 -> return (x1,x2)) >>= flip f (return a) >>= flip f (return b)
  toConstr ctx (_,_) = return $ tuple2Constr
  gunfold ctx k z c | constrIndex c == 1 = z (\mx1 -> return $ \mx2 -> mx1 >>= \x1 -> mx2 >>= \x2 -> return (x1,x2)) >>= k >>= k
  gunfold ctx _ _ _ = error "Data.MData.gunfold(tup2)"
  dataTypeOf ctx _  = return $ tuple2DataType
  dataCast2 ctx f   = liftM gcast2 f

instance (DeepTypeable a,DeepTypeable b) => DeepTypeable (a,b) where
	typeTree (_::Proxy (a,b)) = MkTypeTree (mkName "Prelude.(,)") args [MkConTree (mkName "Prelude.(,)") args]
		where args = [typeTree (Proxy :: Proxy a),typeTree (Proxy :: Proxy b)]

------------------------------------------------------------------------------

tuple3Constr :: Constr
tuple3Constr = mkConstr tuple3DataType "(,,)" [] Infix

tuple3DataType :: DataType
tuple3DataType = mkDataType "Prelude.(,,)" [tuple3Constr]

instance (MData ctx m a, MData ctx m b, MData ctx m c,Sat (ctx (a,b,c))) => MData ctx m (a,b,c) where
  gfoldl ctx f z (a,b,c) = z (\mx1 -> return $ \mx2 -> return $ \mx3 -> mx1 >>= \x1 -> mx2 >>= \x2 -> mx3 >>= \x3 -> return (x1,x2,x3)) >>= flip f (return a) >>= flip f (return b) >>= flip f (return c)
  toConstr ctx (_,_,_) = return $ tuple3Constr
  gunfold ctx k z c | constrIndex c == 1 = z (\mx1 -> return $ \mx2 -> return $ \mx3 -> mx1 >>= \x1 -> mx2 >>= \x2 -> mx3 >>= \x3 -> return (x1,x2,x3)) >>= k >>= k >>= k
  gunfold ctx _ _ _ = error "Data.MData.gunfold(tup3)"
  dataTypeOf ctx _  = return $ tuple3DataType

instance (DeepTypeable a,DeepTypeable b,DeepTypeable c) => DeepTypeable (a,b,c) where
	typeTree (_::Proxy (a,b,c)) = MkTypeTree (mkName "Prelude.(,,)") args [MkConTree (mkName "Prelude.(,,)") args]
		where args = [typeTree (Proxy :: Proxy a),typeTree (Proxy :: Proxy b),typeTree (Proxy :: Proxy c)]

----------------------------------------------------------------------------

tuple4Constr :: Constr
tuple4Constr = mkConstr tuple4DataType "(,,,)" [] Infix

tuple4DataType :: DataType
tuple4DataType = mkDataType "Prelude.(,,,)" [tuple4Constr]

instance (MData ctx m a, MData ctx m b, MData ctx m c, MData ctx m d,Sat (ctx (a,b,c,d)))
         => MData ctx m (a,b,c,d) where
  gfoldl ctx f z (a,b,c,d) = z (\mx1 -> return $ \mx2 -> return $ \mx3 -> return $ \mx4 -> mx1 >>= \x1 -> mx2 >>= \x2 -> mx3 >>= \x3 -> mx4 >>= \x4 -> return (x1,x2,x3,x4)) >>= flip f (return a) >>= flip f (return b) >>= flip f (return c) >>= flip f (return d)
  toConstr ctx (_,_,_,_) = return $ tuple4Constr
  gunfold ctx k z c = case constrIndex c of
                    1 -> z (\mx1 -> return $ \mx2 -> return $ \mx3 -> return $ \mx4 -> mx1 >>= \x1 -> mx2 >>= \x2 -> mx3 >>= \x3 -> mx4 >>= \x4 -> return (x1,x2,x3,x4)) >>= k >>= k >>= k >>= k
                    _ -> error "Data.MData.gunfold(tup4)"
  dataTypeOf ctx _ = return $ tuple4DataType

instance (DeepTypeable a,DeepTypeable b,DeepTypeable c,DeepTypeable d) => DeepTypeable (a,b,c,d) where
	typeTree (_::Proxy (a,b,c,d)) = MkTypeTree (mkName "Prelude.(,,,)") args [MkConTree (mkName "Prelude.(,,,)") args]
		where args = [typeTree (Proxy :: Proxy a),typeTree (Proxy :: Proxy b),typeTree (Proxy :: Proxy c),typeTree (Proxy :: Proxy d)]

------------------------------------------------------------------------------

tuple5Constr :: Constr
tuple5Constr = mkConstr tuple5DataType "(,,,,)" [] Infix

tuple5DataType :: DataType
tuple5DataType = mkDataType "Prelude.(,,,,)" [tuple5Constr]

instance (MData ctx m a, MData ctx m b, MData ctx m c, MData ctx m d, MData ctx m e,Sat (ctx (a,b,c,d,e)))
         => MData ctx m (a,b,c,d,e) where
  gfoldl ctx f z (a,b,c,d,e) = z (\mx1 -> return $ \mx2 -> return $ \mx3 -> return $ \mx4 -> return $ \mx5 -> mx1 >>= \x1 -> mx2 >>= \x2 -> mx3 >>= \x3 -> mx4 >>= \x4 -> mx5 >>= \x5 -> return (x1,x2,x3,x4,x5)) >>= flip f (return a) >>= flip f (return b) >>= flip f (return c) >>= flip f (return d) >>= flip f (return e)
  toConstr ctx (_,_,_,_,_) = return $ tuple5Constr
  gunfold ctx k z c = case constrIndex c of
                    1 -> z (\mx1 -> return $ \mx2 -> return $ \mx3 -> return $ \mx4 -> return $ \mx5 -> mx1 >>= \x1 -> mx2 >>= \x2 -> mx3 >>= \x3 -> mx4 >>= \x4 -> mx5 >>= \x5 -> return (x1,x2,x3,x4,x5)) >>= k >>= k >>= k >>= k >>= k
                    _ -> error "Data.MData.gunfold(tup5)"
  dataTypeOf ctx _ = return $ tuple5DataType

instance (DeepTypeable a,DeepTypeable b,DeepTypeable c,DeepTypeable d,DeepTypeable e) => DeepTypeable (a,b,c,d,e) where
	typeTree (_::Proxy (a,b,c,d,e)) = MkTypeTree (mkName "Prelude.(,,,,)") args [MkConTree (mkName "Prelude.(,,,,)") args]
		where args = [typeTree (Proxy :: Proxy a),typeTree (Proxy :: Proxy b),typeTree (Proxy :: Proxy c),typeTree (Proxy :: Proxy d),typeTree (Proxy :: Proxy e)]

------------------------------------------------------------------------------

tuple6Constr :: Constr
tuple6Constr = mkConstr tuple6DataType "(,,,,,)" [] Infix

tuple6DataType :: DataType
tuple6DataType = mkDataType "Prelude.(,,,,,)" [tuple6Constr]

instance (MData ctx m a, MData ctx m b, MData ctx m c, MData ctx m d, MData ctx m e, MData ctx m f,Sat (ctx (a,b,c,d,e,f)))
         => MData ctx m (a,b,c,d,e,f) where
  gfoldl ctx f z (a,b,c,d,e,f') = z (\mx1 -> return $ \mx2 -> return $ \mx3 -> return $ \mx4 -> return $ \mx5 -> return $ \mx6 -> mx1 >>= \x1 -> mx2 >>= \x2 -> mx3 >>= \x3 -> mx4 >>= \x4 -> mx5 >>= \x5 -> mx6 >>= \x6 -> return (x1,x2,x3,x4,x5,x6)) >>= flip f (return a) >>= flip f (return b) >>= flip f (return c) >>= flip f (return d) >>= flip f (return e) >>= flip f (return f')
  toConstr ctx (_,_,_,_,_,_) = return $ tuple6Constr
  gunfold ctx k z c = case constrIndex c of
                    1 -> z (\mx1 -> return $ \mx2 -> return $ \mx3 -> return $ \mx4 -> return $ \mx5 -> return $ \mx6 -> mx1 >>= \x1 -> mx2 >>= \x2 -> mx3 >>= \x3 -> mx4 >>= \x4 -> mx5 >>= \x5 -> mx6 >>= \x6 -> return (x1,x2,x3,x4,x5,x6)) >>= k >>= k >>= k >>= k >>= k >>= k
                    _ -> error "Data.MData.gunfold(tup6)"
  dataTypeOf ctx _ = return $ tuple6DataType

instance (DeepTypeable a,DeepTypeable b,DeepTypeable c,DeepTypeable d,DeepTypeable e,DeepTypeable f) => DeepTypeable (a,b,c,d,e,f) where
	typeTree (_::Proxy (a,b,c,d,e,f)) = MkTypeTree (mkName "Prelude.(,,,,,)") args [MkConTree (mkName "Prelude.(,,,,,)") args]
		where args = [typeTree (Proxy :: Proxy a),typeTree (Proxy :: Proxy b),typeTree (Proxy :: Proxy c),typeTree (Proxy :: Proxy d),typeTree (Proxy :: Proxy e),typeTree (Proxy :: Proxy f)]

------------------------------------------------------------------------------

tuple7Constr :: Constr
tuple7Constr = mkConstr tuple7DataType "(,,,,,,)" [] Infix

tuple7DataType :: DataType
tuple7DataType = mkDataType "Prelude.(,,,,,,)" [tuple7Constr]

instance (MData ctx m a, MData ctx m b, MData ctx m c, MData ctx m d, MData ctx m e, MData ctx m f, MData ctx m g,Sat (ctx (a,b,c,d,e,f,g)))
         => MData ctx m (a,b,c,d,e,f,g) where
  gfoldl ctx f z (a,b,c,d,e,f',g) =
    z (\mx1 -> return $ \mx2 -> return $ \mx3 -> return $ \mx4 -> return $ \mx5 -> return $ \mx6 -> return $ \mx7 -> mx1 >>= \x1 -> mx2 >>= \x2 -> mx3 >>= \x3 -> mx4 >>= \x4 -> mx5 >>= \x5 -> mx6 >>= \x6 -> mx7 >>= \x7 -> return (x1,x2,x3,x4,x5,x6,x7)) >>= flip f (return a) >>= flip f (return b) >>= flip f (return c) >>= flip f (return d) >>= flip f (return e) >>= flip f (return f') >>= flip f (return g)
  toConstr ctx  (_,_,_,_,_,_,_) = return $ tuple7Constr
  gunfold ctx k z c = case constrIndex c of
                    1 -> z (\mx1 -> return $ \mx2 -> return $ \mx3 -> return $ \mx4 -> return $ \mx5 -> return $ \mx6 -> return $ \mx7 -> mx1 >>= \x1 -> mx2 >>= \x2 -> mx3 >>= \x3 -> mx4 >>= \x4 -> mx5 >>= \x5 -> mx6 >>= \x6 -> mx7 >>= \x7 -> return (x1,x2,x3,x4,x5,x6,x7)) >>= k >>= k >>= k >>= k >>= k >>= k >>= k
                    _ -> error "Data.MData.gunfold(tup7)"
  dataTypeOf ctx _ = return $ tuple7DataType

instance (DeepTypeable a,DeepTypeable b,DeepTypeable c,DeepTypeable d,DeepTypeable e,DeepTypeable f,DeepTypeable g) => DeepTypeable (a,b,c,d,e,f,g) where
	typeTree (_::Proxy (a,b,c,d,e,f,g)) = MkTypeTree (mkName "Prelude.(,,,,,,)") args [MkConTree (mkName "Prelude.(,,,,,,)") args]
		where args = [typeTree (Proxy :: Proxy a),typeTree (Proxy :: Proxy b),typeTree (Proxy :: Proxy c),typeTree (Proxy :: Proxy d),typeTree (Proxy :: Proxy e),typeTree (Proxy :: Proxy f),typeTree (Proxy :: Proxy g)]

------------------------------------------------------------------------------

instance (MData ctx m a, Typeable a,Sat (ctx (Ptr a))) => MData ctx m (Ptr a) where
  toConstr ctx _   = error "Data.MData.toConstr(Ptr)"
  gunfold ctx _ _  = error "Data.MData.gunfold(Ptr)"
  dataTypeOf ctx _ = return $ mkNoRepType "GHC.Ptr.Ptr"
  dataCast1 ctx x  = liftM gcast1 x

instance DeepTypeable Ptr where
	typeTree _ = MkTypeTree (mkName "GHC.Ptr.Ptr") [] []

instance DeepTypeable a => DeepTypeable (Ptr a) where
	typeTree (_::Proxy (Ptr a)) = MkTypeTree (mkName "GHC.Ptr.Ptr") [typeTree (Proxy::Proxy a)] [MkConTree (mkName "GHC.Ptr.Ptr") [typeTree (Proxy::Proxy a)]]

------------------------------------------------------------------------------

instance (MData ctx m a, Typeable a,Sat (ctx (ForeignPtr a))) => MData ctx m (ForeignPtr a) where
  toConstr ctx _   = error "Data.MData.toConstr(ForeignPtr)"
  gunfold ctx _ _  = error "Data.MData.gunfold(ForeignPtr)"
  dataTypeOf ctx _ = return $ mkNoRepType "GHC.ForeignPtr.ForeignPtr"
  dataCast1 ctx x  = liftM gcast1 x

instance DeepTypeable ForeignPtr where
	typeTree _ = MkTypeTree (mkName "GHC.ForeignPtr.ForeignPtr") [] []

instance DeepTypeable a => DeepTypeable (ForeignPtr a) where
	typeTree (_::Proxy (ForeignPtr a)) = MkTypeTree (mkName "GHC.ForeignPtr.ForeignPtr") [typeTree (Proxy::Proxy a)] [MkConTree (mkName "GHC.ForeignPtr.ForeignPtr") [typeTree (Proxy::Proxy a)]]

------------------------------------------------------------------------------
-- The Data instance for Array preserves data abstraction at the cost of 
-- inefficiency. We omit reflection services for the sake of data abstraction.
instance (Typeable a, MData ctx m a, MData ctx m b, Ix a,Sat (ctx (Array a b)),Sat (ctx [b])) => MData ctx m (Array a b)
 where
  gfoldl ctx f z a = z (liftM (listArray (bounds a))) >>= flip f (return $ elems a)
  toConstr ctx _   = error "Data.MData.toConstr(Array)"
  gunfold ctx _ _  = error "Data.MData.gunfold(Array)"
  dataTypeOf ctx _ = return $ mkNoRepType "Data.Array.Array"
  dataCast2 ctx x  = liftM gcast2 x

instance DeepTypeable Array where
	typeTree _ = MkTypeTree (mkName "Data.Array.Array") [] []

instance (DeepTypeable a,DeepTypeable b) => DeepTypeable (Array a b) where
	typeTree (_::Proxy (Array a b)) = MkTypeTree (mkName "Data.Array.Array") [typeTree (Proxy::Proxy a),typeTree (Proxy::Proxy b)] [MkConTree (mkName "Data.Array.fromList") [typeTree (Proxy::Proxy [(a,b)])]]

----------------------------------------------------------------------------
-- Data instance for Proxy

proxyConstr :: Constr
proxyConstr = mkConstr proxyDataType "Proxy" [] Prefix

proxyDataType :: DataType
proxyDataType = mkDataType "Data.Proxy.Proxy" [proxyConstr]

instance (MData ctx m t,Sat (ctx (Proxy t))) => MData ctx m (Proxy t) where
  gfoldl ctx _ z _  = z Proxy
  toConstr ctx _  = return proxyConstr
  gunfold ctx _ z c = case constrIndex c of
                    1 -> z Proxy
                    _ -> error "Data.MData.gunfold(Proxy)"
  dataTypeOf ctx _ = return proxyDataType
  dataCast1 ctx f  = liftM gcast1 f

instance DeepTypeable Proxy where
	typeTree _ = MkTypeTree (mkName "Data.Proxy.Proxy") [] []

instance (Typeable a) => DeepTypeable (Proxy a) where
	typeTree (_::Proxy (Proxy a)) = error "no typeTree for Proxy"

-----------------------------------------------------------------------
-- instance for (:~:)

reflConstr :: Constr
reflConstr = mkConstr equalityDataType "Refl" [] Prefix

equalityDataType :: DataType
equalityDataType = mkDataType "Data.Type.Equality.(:~:)" [reflConstr]

instance (a ~ b, MData ctx m a,Sat (ctx (a :~: b))) => MData ctx m (a :~: b) where
  gfoldl ctx _ z Refl = z Refl
  toConstr ctx Refl   = return reflConstr
  gunfold ctx _ z c   = case constrIndex c of
                      1 -> z Refl
                      _ -> error "Data.MData.gunfold(:~:)"
  dataTypeOf ctx _    = return equalityDataType
  dataCast2 ctx f     = liftM gcast2 f

instance DeepTypeable (:~:) where
	typeTree _ = MkTypeTree (mkName "Data.Type.Equality.(:~:)") [] []

instance (Typeable a,Typeable b) => DeepTypeable (a :~: b) where
	typeTree (_::Proxy (a :~: b)) = error "no typeTree instance for :~:"

-----------------------------------------------------------------------
-- instance for Coercion

coercionConstr :: Constr
coercionConstr = mkConstr equalityDataType "Coercion" [] Prefix

coercionDataType :: DataType
coercionDataType = mkDataType "Data.Type.Coercion.Coercion" [coercionConstr]

instance (Coercible a b, MData ctx m a, MData ctx m b,Sat (ctx (Coercion a b))) => MData ctx m (Coercion a b) where
  gfoldl ctx _ z Coercion = z Coercion
  toConstr ctx Coercion = return coercionConstr
  gunfold ctx _ z c   = case constrIndex c of
                      1 -> z Coercion
                      _ -> error "Data.MData.gunfold(Coercion)"
  dataTypeOf ctx _    = return coercionDataType
  dataCast2 ctx f     = liftM gcast2 f

instance DeepTypeable Coercion where
	typeTree _ = MkTypeTree (mkName "Data.Type.Coercion.Coercion") [] []

instance (Typeable a,Typeable b) => DeepTypeable (Coercion a b) where
	typeTree = error "no typeTree instance for Coercion"

-----------------------------------------------------------------------
-- instance for Data.Version

versionConstr :: Constr
versionConstr = mkConstr versionDataType "Version" ["versionBranch","versionTags"] Prefix

versionDataType :: DataType
versionDataType = mkDataType "Data.Version.Version" [versionConstr]

instance (Monad m,Sat (ctx Version),Sat (ctx [String]),Sat (ctx String),Sat (ctx Char),Sat (ctx [Int]),Sat (ctx Int)) => MData ctx m Version where
  gfoldl ctx k z (Version bs ts) = z (\mx -> return $ \my -> mx >>= \x -> my >>= \y -> return $ Version x y) >>= flip k (return bs) >>= flip k (return ts)
  toConstr ctx (Version _ _) = return versionConstr
  gunfold ctx k z c = case constrIndex c of
                    1 -> z (\mx -> return $ \my -> mx >>= \x -> my >>= \y -> return $ Version x y) >>= k >>= k
                    _ -> error "Data.MData.gunfold(Version)"
  dataTypeOf ctx _  = return versionDataType

$( derive makeDeepTypeable ''Version )

{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, UndecidableInstances, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, StandaloneDeriving, CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Instances
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2004
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (uses Data.Data)
--
-- \"Scrap your boilerplate\" --- Generic programming in Haskell 
-- See <http://www.cs.uu.nl/wiki/GenericProgramming/SYB>. The present module
-- contains thirteen 'Data' instances which are considered dubious (either
-- because the types are abstract or just not meant to be traversed).
-- Instances in this module might change or disappear in future releases
-- of this package. 
--
-- (This module does not export anything. It really just defines instances.)
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.WithClass.MGenerics.Instances () where

------------------------------------------------------------------------------

import Data.Ratio
import Data.DeriveTH                 -- Library for deriving instances for existing types
import Data.DeepTypeable
import Data.WithClass.Derive.DeepTypeable

import Data.Data (Data,Constr(..),DataType(..),DataRep(..),showConstr,constrIndex,dataTypeConstrs,mkConstr,mkDataType,Fixity(..),ConstrRep(..),constrRep,indexConstr,mkNoRepType,mkIntegralConstr,mkIntType,mkRealConstr,mkFloatType,mkCharConstr,mkCharType)
import Language.Haskell.Exts as H hiding (Fixity)
import Language.Haskell.TH.Syntax hiding (Fixity)
import Data.WithClass.MData
import Data.Generics.Instances
import Control.Monad
import Control.Monad.IO.Class
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Set (Set(..))
import qualified Data.Set as Set

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ >= 611
import GHC.IO.Handle         -- So we can give Data instance for Handle
#else
import GHC.IOBase            -- So we can give Data instance for IO, Handle
#endif
import GHC.Stable            -- So we can give Data instance for StablePtr
import GHC.ST                -- So we can give Data instance for ST
import GHC.Conc              -- So we can give Data instance for TVar
import Data.IORef            -- So we can give Data instance for IORef
import Control.Concurrent    -- So we can give Data instance for MVar
#else
# ifdef __HUGS__
import Hugs.Prelude( Ratio(..) )
# endif
import System.IO
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.StablePtr
import Control.Monad.ST
#endif

-- Version compatibility issues caused by #2760
myMkNoRepType :: String -> DataType
#if __GLASGOW_HASKELL__ >= 611
myMkNoRepType = mkNoRepType
#else
myMkNoRepType = mkNorepType
#endif


------------------------------------------------------------------------------
--
--      Instances of the Data class for Prelude-like types.
--      We define top-level definitions for representations.
--
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- Instances of abstract datatypes (6)
------------------------------------------------------------------------------

instance (Monad m,Sat (ctx TypeRep)) => MData ctx m TypeRep where
  toConstr ctx _   = error "toConstr"
  gunfold ctx _ _  = error "gunfold"
  dataTypeOf ctx _ = return $ myMkNoRepType "Data.Typeable.TypeRep"

$( derive makeDeepTypeableAbstract ''TypeRep )

------------------------------------------------------------------------------

instance (Monad m,Sat (ctx TyCon)) => MData ctx m TyCon where
  toConstr ctx _   = error "toConstr"
  gunfold ctx _ _  = error "gunfold"
  dataTypeOf ctx _ = return $ myMkNoRepType "Data.Typeable.TyCon"

$( derive makeDeepTypeableAbstract ''TyCon )

------------------------------------------------------------------------------

instance (Monad m,Sat (ctx DataType)) => MData ctx m DataType where
  toConstr ctx _   = error "toConstr"
  gunfold ctx _ _  = error "gunfold"
  dataTypeOf ctx _ = return $ myMkNoRepType "Data.Generics.Basics.DataType"

$( derive makeDeepTypeable ''DataType )

instance (Monad m,Sat (ctx DataRep)) => MData ctx m DataRep where
  toConstr ctx _   = error "toConstr"
  gunfold ctx _ _  = error "gunfold"
  dataTypeOf ctx _ = return $ myMkNoRepType "Data.Generics.Basics.DataRep"

deriving instance Typeable DataRep

$( derive makeDeepTypeable ''DataRep )

instance (Monad m,Sat (ctx Constr)) => MData ctx m Constr where
  toConstr ctx _   = error "toConstr"
  gunfold ctx _ _  = error "gunfold"
  dataTypeOf ctx _ = return $ myMkNoRepType "Data.Generics.Basics.Constr"

deriving instance Typeable Constr

$( derive makeDeepTypeable ''Constr )

instance (Monad m,Sat (ctx ConstrRep)) => MData ctx m ConstrRep where
  toConstr ctx _   = error "toConstr"
  gunfold ctx _ _  = error "gunfold"
  dataTypeOf ctx _ = return $ myMkNoRepType "Data.Generics.Basics.ConstrRep"

deriving instance Typeable ConstrRep

$( derive makeDeepTypeable ''ConstrRep )

instance (Monad m,Sat (ctx Fixity)) => MData ctx m Fixity where
  toConstr ctx _   = error "toConstr"
  gunfold ctx _ _  = error "gunfold"
  dataTypeOf ctx _ = return $ myMkNoRepType "Data.Generics.Basics.Fixity"

deriving instance Typeable Fixity

$( derive makeDeepTypeable ''Fixity )

------------------------------------------------------------------------------

instance (Monad m,Sat (ctx Handle)) => MData ctx m Handle where
  toConstr ctx _   = error "toConstr"
  gunfold ctx _ _  = error "gunfold"
  dataTypeOf ctx _ = return $ myMkNoRepType "GHC.IOBase.Handle"

$( derive makeDeepTypeableAbstract ''Handle )

------------------------------------------------------------------------------

instance (DeepTypeable a,Monad m,Sat (ctx (StablePtr a))) => MData ctx m (StablePtr a) where
  toConstr ctx _   = error "toConstr"
  gunfold ctx _ _  = error "gunfold"
  dataTypeOf ctx _ = return $ myMkNoRepType "GHC.Stable.StablePtr"

instance DeepTypeable StablePtr where
	typeTree _ = MkTypeTree (mkName "GHC.Stable.StablePtr") [] []

instance (DeepTypeable a) => DeepTypeable (StablePtr a) where
	typeTree (_::Proxy (StablePtr a)) = MkTypeTree (mkName "GHC.Stable.StablePtr") [typeTree (Proxy::Proxy a)] [MkConTree (mkName "GHC.Stable.StablePtr") [typeTree (Proxy::Proxy a)]]

------------------------------------------------------------------------------

#ifdef __GLASGOW_HASKELL__
instance (Monad m,Sat (ctx ThreadId)) => MData ctx m ThreadId where
  toConstr ctx _   = error "toConstr"
  gunfold ctx _ _  = error "gunfold"
  dataTypeOf ctx _ = return $ myMkNoRepType "GHC.Conc.ThreadId"
#endif

instance DeepTypeable ThreadId

------------------------------------------------------------------------------
-- Dubious instances (7)
------------------------------------------------------------------------------

#ifdef __GLASGOW_HASKELL__
instance (DeepTypeable a,Monad m,Sat (ctx (TVar a))) => MData ctx m (TVar a) where
  toConstr ctx _   = error "toConstr"
  gunfold ctx _ _  = error "gunfold"
  dataTypeOf ctx _ = return $ myMkNoRepType "GHC.Conc.TVar"
#endif

instance DeepTypeable TVar where
	typeTree _ = MkTypeTree (mkName "GHC.Conc.TVar") [] []

instance (DeepTypeable a) => DeepTypeable (TVar a) where
	typeTree (_::Proxy (TVar a)) = MkTypeTree (mkName "GHC.Conc.TVar") [typeTree (Proxy::Proxy a)] [MkConTree (mkName "GHC.Conc.newTVar") [typeTree (Proxy::Proxy a)]]

------------------------------------------------------------------------------

instance (DeepTypeable a,Monad m,Sat (ctx (MVar a))) => MData ctx m (MVar a) where
  toConstr ctx _   = error "toConstr"
  gunfold ctx _ _  = error "gunfold"
  dataTypeOf ctx _ = return $ myMkNoRepType "GHC.Conc.MVar"

instance DeepTypeable MVar where
	typeTree _ = MkTypeTree (mkName "GHC.Conc.MVar") [] []

instance (DeepTypeable a) => DeepTypeable (MVar a) where
	typeTree (_::Proxy (MVar a)) = MkTypeTree (mkName "GHC.Conc.MVar") [typeTree (Proxy::Proxy a)] [MkConTree (mkName "GHC.Conc.newMVar") [typeTree (Proxy::Proxy a)]]

------------------------------------------------------------------------------

#ifdef __GLASGOW_HASKELL__
instance (DeepTypeable a,Monad m,Sat (ctx (STM a))) => MData ctx m (STM a) where
  toConstr ctx _   = error "toConstr"
  gunfold ctx _ _  = error "gunfold"
  dataTypeOf ctx _ = return $ myMkNoRepType "GHC.Conc.STM"
#endif

instance DeepTypeable STM where
	typeTree _ = MkTypeTree (mkName "GHC.Conc.STM") [] []

instance (DeepTypeable a) => DeepTypeable (STM a) where
	typeTree (_::Proxy (STM a)) = MkTypeTree (mkName "GHC.Conc.STM") [typeTree (Proxy::Proxy a)] [MkConTree (mkName "GHC.Conc.STM") [typeTree (Proxy::Proxy a)]]

------------------------------------------------------------------------------

instance (DeepTypeable s, DeepTypeable a,Monad m,Sat (ctx (ST s a))) => MData ctx m (ST s a) where
  toConstr ctx _   = error "toConstr"
  gunfold ctx _ _  = error "gunfold"
  dataTypeOf ctx _ = return $ myMkNoRepType "GHC.ST.ST"

instance DeepTypeable ST where
	typeTree _ = MkTypeTree (mkName "GHC.ST.ST") [] []

instance (DeepTypeable s,DeepTypeable a) => DeepTypeable (ST s a) where
	typeTree (_::Proxy (ST s a)) = MkTypeTree (mkName "GHC.ST.ST") [typeTree (Proxy::Proxy s),typeTree (Proxy::Proxy a)] [MkConTree (mkName "GHC.ST.ST") [typeTree (Proxy::Proxy s),typeTree (Proxy::Proxy a)]]

------------------------------------------------------------------------------

-- instances of this kind are the motivation for the generalization of this package
instance (DeepTypeable a, m,MData ctx m a,Sat (ctx (IORef a))) => MData ctx m (IORef a) where
	gfoldl ctx k z ior = liftIO (readIORef ior) >>= \x -> z (\mx -> mx >>= liftIO . newIORef) >>= flip k (return x)
	toConstr ctx x   = dataTypeOf ctx x >>= \t -> return $ indexConstr t 1
	gunfold ctx k z c = case constrIndex c of
		1 -> z (\mx -> mx >>= liftIO . newIORef) >>= k
	dataTypeOf ctx _ = return ty
		where ty = mkDataType "Data.IORef" [mkConstr ty "IORef" [] Prefix]

instance DeepTypeable IORef where
	typeTree _ = MkTypeTree (mkName "Data.IORef") [] []

instance (DeepTypeable a) => DeepTypeable (IORef a) where
	typeTree (_::Proxy (IORef a)) = MkTypeTree (mkName "Data.IORef") [typeTree (Proxy::Proxy a)] [MkConTree (mkName "Data.IORef.newIORef") [typeTree (Proxy::Proxy a)]]

------------------------------------------------------------------------------

instance (Monad m,DeepTypeable a,Sat (ctx (IO a))) => MData ctx m (IO a) where
  toConstr ctx _   = error "toConstr"
  gunfold ctx _ _  = error "gunfold"
  dataTypeOf ctx _ = return $ myMkNoRepType "GHC.IOBase.IO"

instance DeepTypeable IO where
	typeTree _ = MkTypeTree (mkName "GHC.IOBase.IO") [] []

instance (DeepTypeable a) => DeepTypeable (IO a) where
	typeTree (_::Proxy (IO a)) = MkTypeTree (mkName "GHC.IOBase.IO") [typeTree (Proxy::Proxy a)] [MkConTree (mkName "GHC.IOBase.IO") [typeTree (Proxy::Proxy a)]]

------------------------------------------------------------------------------

--
-- A last resort for functions
--

instance (MData ctx m a, MData ctx m b,Sat (ctx (a -> b))) => MData ctx m (a -> b) where
  toConstr ctx _   = error "toConstr"
  gunfold ctx _ _  = error "gunfold"
  dataTypeOf ctx _ = return $ myMkNoRepType "Prelude.(->)"
  dataCast2 ctx f  = liftM gcast2 f

instance DeepTypeable (->) where
	typeTree _ = MkTypeTree (mkName "Prelude.(->)") [] []

instance (DeepTypeable a,DeepTypeable b) => DeepTypeable (a -> b) where
	typeTree (_::Proxy (a -> b)) = MkTypeTree (mkName "Prelude.(->)") [typeTree (Proxy::Proxy a),typeTree (Proxy::Proxy b)] [MkConTree (mkName "Prelude.(->)") [typeTree (Proxy::Proxy a),typeTree (Proxy::Proxy b)]]

instance (MData ctx m k, MData ctx m a, Ord k,Sat (ctx (k, a)),Sat (ctx [(k, a)]),Sat (ctx (Map k a))) => MData ctx m (Map k a) where
  gfoldl ctx f z m   = z (liftM Map.fromList) >>= flip f (return $ Map.toList m)
  toConstr ctx _     = return $ mapFromListConstr
  gunfold ctx k z c  = case constrIndex c of
    1 -> z (liftM Map.fromList) >>= k
    _ -> error "gunfold"
  dataTypeOf ctx _   = return $ mapDataType
  dataCast2 ctx f    = liftM gcast2 f

mapFromListConstr :: Constr
mapFromListConstr = mkConstr mapDataType "fromList" [] Prefix

mapDataType :: DataType
mapDataType = mkDataType "Data.Map.Base.Map" [mapFromListConstr]

instance DeepTypeable Map where
	typeTree _ = MkTypeTree (mkName "Data.Map.Map") [] []

instance (DeepTypeable a,DeepTypeable b) => DeepTypeable (Map a b) where
	typeTree (_::Proxy (Map a b)) = MkTypeTree (mkName "Data.Map.Map") [typeTree (Proxy::Proxy a),typeTree (Proxy::Proxy b)] [MkConTree (mkName "Data.Map.fromList") [typeTree (Proxy::Proxy [(a,b)])]]

instance (MData ctx m k,Ord k,Sat (ctx k),Sat (ctx [k]),Sat (ctx (Set k))) => MData ctx m (Set k) where
  gfoldl ctx f z m   = z (liftM Set.fromList) >>= flip f (return $ Set.toList m)
  toConstr ctx _     = return $ setFromListConstr
  gunfold ctx k z c  = case constrIndex c of
    1 -> z (liftM Set.fromList) >>= k
    _ -> error "gunfold"
  dataTypeOf ctx _   = return $ setDataType
  dataCast1 ctx f    = liftM gcast1 f

setFromListConstr :: Constr
setFromListConstr = mkConstr setDataType "fromList" [] Prefix

setDataType :: DataType
setDataType = mkDataType "Data.Set.Base.Set" [setFromListConstr]

instance DeepTypeable Set where
	typeTree _ = MkTypeTree (mkName "Data.Set.Set") [] []

instance (DeepTypeable a) => DeepTypeable (Set a) where
	typeTree (_::Proxy (Set a)) = MkTypeTree (mkName "Data.Set.Set") [typeTree (Proxy::Proxy a)] [MkConTree (mkName "Data.Set.fromList") [typeTree (Proxy::Proxy [a])]]

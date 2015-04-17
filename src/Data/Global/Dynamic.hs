-- -*- encoding: utf-8; fill-column: 95 -*-

{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, GADTs, CPP, ScopedTypeVariables #-}
-----------------------------------------------------------------------------------------------
-- |
-- Module        : Data.Global.Registry
-- Creation Date : 2011-09-01
-- Authors       : Jean-Marie Gaillourdet <jmg@gaillourdet.net>
-- License       : BSD-style
-- Portability   : all
--
-- The internal module.
-----------------------------------------------------------------------------------------------
module Data.Global.Dynamic (
  -- * Public Interface
    declareIORef, declareMVar, declareEmptyMVar, declareTVar, declareBasicHashTable, declareCMap, declareWeakBasicHashTable, declareWeakMap

  -- * Private Testing Interface
  , lookupOrInsert
  , setupRegistry
  , globalRegistry
) where

import qualified System.Mem.WeakMap as WeakMap
import Control.Concurrent.MVar ( MVar, newMVar, newEmptyMVar, modifyMVar )
import Control.Concurrent.STM  ( TVar, newTVarIO )
import Data.IORef
import Data.Dynamic
import Data.HashMap.Strict as M
import GHC.Conc                ( pseq )
import Data.Hashable
import GHC.IO                  ( unsafePerformIO, unsafeDupablePerformIO )

import Data.HashTable.Weak.IO as Weak
import Data.HashTable.IO as HashIO
import Data.HashTable.ST.Basic as BasicHashST
import System.Mem.StableName.Exts
import qualified Control.Concurrent.Map as CMap
import Unsafe.Coerce

type Registry = HashMap (TypeRep,DynamicHashableEq) Dyn

data DynamicHashableEq where
	DynHashableEq :: (Typeable a,Eq a,Hashable a) => a -> DynamicHashableEq

data Dyn where
	Dyn :: Typeable a => a -> Dyn

unsafeFromDyn :: Typeable a => Dyn -> a
--unsafeFromDyn (Dyn a) = unsafeCoerce a
unsafeFromDyn (Dyn x) = case cast x of
	Nothing -> error "type cast"
	Just y -> y

instance Hashable DynamicHashableEq where
	hashWithSalt i (DynHashableEq a) = hashWithSalt i a
instance Eq DynamicHashableEq where
	(DynHashableEq x) == (DynHashableEq (y::y)) = case cast x :: Maybe y of
		Nothing -> False
		Just y' -> y == y'

toDynHashableEq :: (Eq a,Hashable a,Typeable a) => a -> DynamicHashableEq
toDynHashableEq = DynHashableEq

fromDynamicHashableEq :: (Eq a,Hashable a,Typeable a) => DynamicHashableEq -> Maybe a
fromDynamicHashableEq (DynHashableEq x) = cast x

-- | Test helper
setupRegistry :: IO (MVar Registry)
setupRegistry = m `pseq` newMVar m
  where
	m = M.empty



{-# NOINLINE globalRegistry #-}
globalRegistry :: MVar Registry
globalRegistry = m `pseq` unsafePerformIO (newMVar m)
  where
    m = M.empty


-- TODO: add a proper assertion explaining the problem

-- | Exposed for unit testing
lookupOrInsert
    :: forall a. forall ref. forall b. (Typeable b,Hashable b,Eq b,Typeable a, Typeable ref)
    => MVar Registry
    -> (a -> IO (ref a))
    -> b
    -> a
    -> IO (ref a)
lookupOrInsert registry new name _
    | registry `pseq` new `pseq` name `pseq` False = undefined
lookupOrInsert registry (new :: a -> IO (ref a)) name val = modifyMVar registry lkup
  where
	err = error "Data.Global.Registry: Invariant violation"
	
	typ = typeOf (undefined :: ref a)
	
	lkup :: Registry -> IO (Registry, ref a)
	lkup reg = case M.lookup (typ,toDynHashableEq name) reg of
		Just ref -> return (reg, unsafeFromDyn ref)
		Nothing -> do
			ref <- new val
			return (M.insert (typ,toDynHashableEq name) (Dyn ref) reg, ref)
{-# NOINLINE lookupOrInsert #-}

lookupOrInsert2
	:: forall a1. forall a2. forall c. forall b. (Typeable a1,Typeable a2,Typeable b,Hashable b,Eq b,Typeable c)
	=> MVar Registry
	-> (a1 -> a2 -> IO (c a1 a2))
	-> b
	-> a1 -> a2
	-> IO (c a1 a2)
lookupOrInsert2 registry new name _ _
    | registry `pseq` new `pseq` name `pseq` False = undefined
lookupOrInsert2 registry (new :: a1 -> a2 -> IO (c a1 a2)) name val1 val2 = modifyMVar registry lkup
  where
    err = error $ "Data.Global.Registry: Invariant violation"

    typ = typeOf (undefined :: c a1 a2)

    lkup :: Registry -> IO (Registry, c a1 a2)
    lkup reg = case M.lookup (typ,toDynHashableEq name) reg of
        Just ref -> return (reg, unsafeFromDyn ref)
        Nothing ->
         do { ref <- new val1 val2
            ; return (M.insert (typ,toDynHashableEq name) (Dyn ref) reg, ref)
            }


lookupOrInsertIORef
    :: (Typeable a,Hashable b,Typeable b,Eq b)
    => b
    -> a
    -> IO (IORef a)
lookupOrInsertIORef = lookupOrInsert globalRegistry newIORef
{-# NOINLINE lookupOrInsertIORef #-}



lookupOrInsertMVar
    :: (Typeable a,Typeable b,Hashable b,Eq b)
    => b
    -> a
    -> IO (MVar a)
lookupOrInsertMVar = lookupOrInsert globalRegistry newMVar
{-# NOINLINE lookupOrInsertMVar #-}



lookupOrInsertEmptyMVar
    :: forall a b. (Typeable a,Typeable b,Eq b,Hashable b)
    => b
    -> IO (MVar a)
lookupOrInsertEmptyMVar name = lookupOrInsert globalRegistry newEmptyMVar' name (undefined :: a)
  where
    newEmptyMVar' :: a -> IO (MVar a)
    newEmptyMVar' _ = newEmptyMVar
{-# NOINLINE lookupOrInsertEmptyMVar #-}

deriving instance Typeable CMap.Map

lookupOrInsertCMap
    :: (Eq name,Hashable name,Typeable name,Typeable k,Typeable v,Typeable b)
    => b -> name
    -> IO (CMap.Map k v)
lookupOrInsertCMap n name = lookupOrInsert2 globalRegistry (\k v -> CMap.empty) name (error "lookupOrInsertCMap") (error "lookupOrInsertCMap")
{-# NOINLINE lookupOrInsertCMap #-}

lookupOrInsertBasicHashTable
    :: (Eq k,Hashable k,Typeable k,Typeable v,Typeable b,Hashable b,Eq b)
    => Int -> b
    -> IO (HashIO.BasicHashTable k v)
lookupOrInsertBasicHashTable size n = lookupOrInsert2 globalRegistry (\k v -> HashIO.newSized size) n (error "lookupOrInsertBasicHashTable") (error "lookupOrInsertBasicHashTable")
{-# NOINLINE lookupOrInsertBasicHashTable #-}

lookupOrInsertWeakBasicHashTable
    :: (Eq k,Hashable k,Typeable k,Typeable v,Typeable b,Hashable b,Eq b)
    => Int -> b
    -> IO (Weak.BasicHashTable k v)
lookupOrInsertWeakBasicHashTable size n = lookupOrInsert2 globalRegistry (\k v -> Weak.newSized size) n (error "lookupOrInsertBasicHashTable") (error "lookupOrInsertBasicHashTable")
{-# NOINLINE lookupOrInsertWeakBasicHashTable #-}

lookupOrInsertWeakMap
    :: (Eq k,Hashable k,Typeable k,Typeable v,Typeable b,Hashable b,Eq b)
    => b
    -> IO (WeakMap.WeakMap k v)
lookupOrInsertWeakMap n = lookupOrInsert2 globalRegistry (\k v -> WeakMap.new) n (error "lookupOrInsertBasicHashTable") (error "lookupOrInsertBasicHashTable")
{-# NOINLINE lookupOrInsertWeakMap #-}

lookupOrInsertTVar
    :: (Typeable a,Typeable b,Eq b,Hashable b)
    => b
    -> a
    -> IO (TVar a)
lookupOrInsertTVar = lookupOrInsert globalRegistry newTVarIO
{-# NOINLINE lookupOrInsertTVar #-}

declareCMap :: (Typeable a,Eq name,Hashable name,Typeable name,Typeable k,Typeable v,Eq k,Hashable k) => a -> name -> (CMap.Map k v)
declareCMap a name = unsafeDupablePerformIO $ lookupOrInsertCMap a name
{-# NOINLINE declareCMap #-}

-- | @declareIORef name val@ maps a variable name to an 'IORef'. Calling it multiple times with the same
-- @name@ and type of 'val' will always return the same 'IORef'.
--
-- @
-- someVar :: IORef Int
-- someVar = declareMVar \"my-global-some-var\" 0
-- @
--
-- Note, there is /no/ need to use 'unsafePerformIO' or to add a @{-\# NOINLINE someVar \#-}@
-- pragma in order to define top-level 'IORef's.
declareIORef
    :: (Typeable a,Typeable b,Eq b,Hashable b)
    => b   -- ^ The identifying name
    -> a        -- ^ The initial value of the 'IORef', it may or may not be used.
    -> IORef a  -- ^ A unique 'IORef' determined by @(name, typeOf val)@. Whether it refers to
                -- the given initial value or not is unspecified.
declareIORef name val = unsafeDupablePerformIO $ lookupOrInsertIORef name val
{-# NOINLINE declareIORef #-}

-- | @declareMVar name val@ maps a variable name to an 'MVar'. Calling it multiple times with the same
-- @name@ and type of 'val' will always return the same 'MVar'.
--
-- @
-- someVar :: MVar Int
-- someVar = declareMVar \"my-global-some-var\" 0
-- @
--
-- Note, there is /no/ need to use 'unsafePerformIO' or to add a @{-\# NOINLINE someVar \#-}@
-- pragma in order to define top-level 'MVar's.
declareMVar
    :: (Typeable a,Typeable b,Eq b,Hashable b)
    => b  -- ^ The identifying name
    -> a       -- ^ The initial value of the 'MVar', it may or may not be used.
    -> MVar a  -- ^ A unique 'MVar' determined by @(name, typeOf val)@. Whether it refers to
               -- the given initial value or not is unspecified.
declareMVar name val = unsafeDupablePerformIO $ lookupOrInsertMVar name val
{-# NOINLINE declareMVar #-}


-- | @declareEmptyMVar name@ maps a variable name to an 'MVar'. Calling it multiple times with
-- the same @name@ and type of the expected 'MVar' will always return the same 'MVar'.
--
-- @
-- someVar :: MVar Int
-- someVar = declareEmptyMVar \"my-global-some-var\"
-- @
--
-- Note, there is /no/ need to use 'unsafePerformIO' or to add a @{-\# NOINLINE someVar \#-}@
-- pragma in order to define top-level 'MVar's.
declareEmptyMVar
    :: (Typeable a,Typeable b,Eq b,Hashable b)
    => b  -- ^ The identifying name
    -> MVar a  -- ^ A unique 'MVar' determined by @(name, typeOf val)@. Whether it is still
               -- empty depends on the rest of the program.
declareEmptyMVar name = unsafeDupablePerformIO $ lookupOrInsertEmptyMVar name
{-# NOINLINE declareEmptyMVar #-}

declareBasicHashTable
    :: (Eq k,Hashable k,Typeable k,Typeable v,Typeable b,Eq b,Hashable b)
    => Int -> b  -- ^ The identifying name
    -> HashIO.BasicHashTable k v 
declareBasicHashTable size name = unsafeDupablePerformIO $ lookupOrInsertBasicHashTable size name
{-# NOINLINE declareBasicHashTable #-}

declareWeakBasicHashTable
    :: (Eq k,Hashable k,Typeable k,Typeable v,Typeable b,Eq b,Hashable b)
    => Int -> b  -- ^ The identifying name
    -> Weak.BasicHashTable k v 
declareWeakBasicHashTable size name = unsafeDupablePerformIO $ lookupOrInsertWeakBasicHashTable size name
{-# NOINLINE declareWeakBasicHashTable #-}

declareWeakMap
    :: (Eq k,Hashable k,Typeable k,Typeable v,Typeable b,Eq b,Hashable b)
    => b  -- ^ The identifying name
    -> WeakMap.WeakMap k v 
declareWeakMap name = unsafeDupablePerformIO $ lookupOrInsertWeakMap name
{-# NOINLINE declareWeakMap #-}

-- | @declareTVar name val@ maps a variable name to an 'TVar'. Calling it multiple times with the same
-- @name@ and type of 'val' will always return the same 'TVar'.
--
-- @
-- someVar :: TVar Int
-- someVar = declareMVar \"my-global-some-var\" 0
-- @
--
-- Note, there is /no/ need to use 'unsafePerformIO' or to add a @{-\# NOINLINE someVar \#-}@
-- pragma in order to define top-level 'TVar's.
declareTVar
    :: (Typeable a,Typeable b,Eq b,Hashable b)
    => b  -- ^ The identifying name
    -> a       -- ^ The initial value of the 'TVar', it may or may not be used.
    -> TVar a  -- ^ A unique 'TVar' determined by @(name, typeOf val)@. Whether it refers to
               -- the given initial value or not is unspecified.
declareTVar name val = unsafeDupablePerformIO $ lookupOrInsertTVar name val
{-# NOINLINE declareTVar #-}

deriving instance Typeable BasicHashST.HashTable

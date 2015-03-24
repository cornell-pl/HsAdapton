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
    declareIORef, declareMVar, declareEmptyMVar, declareTVar, declareIOHashTable, declareWeakTable, declareCMap

  -- * Private Testing Interface
  , lookupOrInsert
  , setupRegistry
  , globalRegistry
) where

import Control.Concurrent.MVar ( MVar, newMVar, newEmptyMVar, modifyMVar )
#if __GLASGOW_HASKELL__ < 702
import Control.Concurrent.MVar ( takeMVar, putMVar )
#endif
import Control.Concurrent.STM  ( TVar, newTVarIO )
#if __GLASGOW_HASKELL__ < 702
import Control.Exception       ( evaluate )
#endif
import Data.IORef
import Data.Dynamic
import Data.HashMap.Strict as M
import GHC.Conc                ( pseq )
import Data.Hashable
import GHC.IO                  ( unsafePerformIO, unsafeDupablePerformIO )
import qualified Data.HashTable.IO as HashIO
import qualified Data.HashTable.ST.Basic as HashST
import System.Mem.WeakTable as WeakTable
import System.Mem.StableName
import qualified Control.Concurrent.Map as CMap



#if __GLASGOW_HASKELL__ >= 702
type Registry = HashMap (TypeRep,TypeRep,DynamicHashableEq) Dynamic
#else
type Registry = HashMap (Int,Int,String) Dynamic
#endif

data DynamicHashableEq where
	DynHashableEq :: (Typeable a,Eq a,Hashable a) => a -> DynamicHashableEq

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
lookupOrInsert registry new name val = modifyMVar registry lkup
  where
    err ex got = error $ "Data.Global.Registry: Invariant violation\n"
                       ++ "expected: " ++ show ex ++ "\n"
                       ++ "got: " ++ show got ++ "\n"

    typVal = typeOf (undefined :: a)
    typRef = typeOf (undefined :: ref ()) -- TypeRep representing the reference, e.g. IORef,
                                          -- MVar

    lkup :: Registry -> IO (Registry, ref a)
    lkup reg = case M.lookup (typRef, typVal, toDynHashableEq name) reg of
        Just ref -> return (reg, fromDyn ref (err typVal (dynTypeRep ref)))
        Nothing ->
         do { ref <- new val
            ; return (M.insert (typRef, typVal, toDynHashableEq name) (toDyn ref) reg, ref)
            }
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
lookupOrInsert2 registry new name val1 val2 = modifyMVar registry lkup
  where
    err ex got = error $ "Data.Global.Registry: Invariant violation\n"
                       ++ "expected: " ++ show ex ++ "\n"
                       ++ "got: " ++ show got ++ "\n"

    typVal = typeOf (undefined :: (a1,a2))
    typRef = typeOf (undefined :: c () ()) -- TypeRep representing the reference, e.g. IORef,
                                          -- MVar

    lkup :: Registry -> IO (Registry, c a1 a2)
    lkup reg = case M.lookup (typRef, typVal, toDynHashableEq name) reg of
        Just ref -> return (reg, fromDyn ref (err typVal (dynTypeRep ref)))
        Nothing ->
         do { ref <- new val1 val2
            ; return (M.insert (typRef, typVal, toDynHashableEq name) (toDyn ref) reg, ref)
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

deriving instance Typeable HashST.HashTable
deriving instance Typeable CMap.Map

lookupOrInsertCMap
    :: (Eq name,Hashable name,Typeable name,Typeable k,Typeable v,Typeable b)
    => b -> name
    -> IO (CMap.Map k v)
lookupOrInsertCMap n name = lookupOrInsert2 globalRegistry (\k v -> CMap.empty) name (error "lookupOrInsertCMap") (error "lookupOrInsertCMap")
{-# NOINLINE lookupOrInsertCMap #-}

lookupOrInsertIOHashTable
    :: (Typeable k,Typeable v,Typeable b,Hashable b,Eq b)
    => b
    -> IO (HashIO.IOHashTable HashST.HashTable k v)
lookupOrInsertIOHashTable n = lookupOrInsert2 globalRegistry (\k v -> HashIO.new) n (error "lookupOrInsertIOHashTable") (error "lookupOrInsertIOHashTable")
{-# NOINLINE lookupOrInsertIOHashTable #-}

lookupOrInsertWeakTable
    :: (Eq name,Hashable name,Eq k,Hashable k,Typeable k,Typeable v,Typeable b,Typeable name)
    => b -> name
    -> IO (WeakTable k v)
lookupOrInsertWeakTable n name = lookupOrInsert2 globalRegistry (\k v -> WeakTable.newFor n) name (error "lookupOrInsertWeakTable") (error "lookupOrInsertWeakTable")
{-# NOINLINE lookupOrInsertWeakTable #-}

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

declareWeakTable :: (Typeable a,Eq name,Hashable name,Typeable name,Typeable k,Typeable v,Eq k,Hashable k) => a -> name -> (WeakTable k v)
declareWeakTable a name = unsafeDupablePerformIO $ lookupOrInsertWeakTable a name
{-# NOINLINE declareWeakTable #-}

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

declareIOHashTable
    :: (Typeable k,Typeable v,Typeable b,Eq b,Hashable b)
    => b  -- ^ The identifying name
    -> HashIO.IOHashTable HashST.HashTable k v 
declareIOHashTable name = unsafeDupablePerformIO $ lookupOrInsertIOHashTable name
{-# NOINLINE declareIOHashTable #-}



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

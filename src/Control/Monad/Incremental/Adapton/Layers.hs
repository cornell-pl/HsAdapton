{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TypeOperators, DeriveDataTypeable, EmptyDataDecls, KindSignatures, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, GeneralizedNewtypeDeriving #-}

module Control.Monad.Incremental.Adapton.Layers where

import Control.Monad.Incremental
import System.Mem.Weak
import Control.Monad.Ref
import Control.Monad.Trans.Class
import Data.IORef
import Data.Maybe
import System.IO.Unsafe
import Safe
import Control.Monad.Incremental.Adapton.Types
import Control.Monad.IO.Class --we should avoid using IO, and use State monads instead of global variables, though that is evidently slower...
import Data.Typeable
import System.Mem.WeakKey
import Control.Monad.Lazy
import Control.Monad.Trans
import Data.Strict.Tuple
import Data.Strict.List as Strict
import Control.Monad

import Data.DeriveTH                 -- Library for deriving instances for existing types
import Data.DeepTypeable
import Data.WithClass.Derive.DeepTypeable
import Language.Haskell.TH.Syntax hiding (lift,Infix,Fixity)
import Data.Strict.Maybe as Strict
import Control.Applicative

import Debug

-- | Type for Adapton incremental computation
data Adapton deriving Typeable

$( derive makeDeepTypeableAbstract ''Adapton )

type Inner = Inside Adapton
type Outer = Outside Adapton

-- * Callstack used by the @Inner@ and @Outer@ monads

topStackThunkElement :: CallStack inc r m -> Maybe (StackElement inc r m)
topStackThunkElement (SCons x xs) = if isThunkStackElement x then Just x else topStackThunkElement xs
topStackThunkElement SNil = Nothing

-- make sure that the stack is strict, to fix a memory leak with lazy stacks
type CallStack inc (r :: * -> *) (m :: * -> *) = SList (StackElement inc r m)
type StackElement inc (r :: * -> *) (m :: * -> *) = (NodeMeta inc r m :!: SMaybe (r (Dependencies inc r m))) -- the @Bool@ denotes the kind of the element: true=thunk, false=ref

showCallStack :: CallStack inc r m -> String
showCallStack = show . Strict.map (\(x :!: _) -> x)

{-# INLINE isThunkStackElement #-}
isThunkStackElement :: StackElement inc r m -> Bool
isThunkStackElement (_ :!: (SJust _)) = True
isThunkStackElement (_ :!: SNothing) = False

{-# NOINLINE callstack #-}
callstack :: IORef (CallStack inc r m)
callstack = unsafePerformIO $ newIORef SNil

{-# INLINE topStack #-}
topStack :: IO (Maybe (StackElement inc r m))
topStack = readIORef callstack >>= \s -> case s of
	SCons x xs -> return $ Just x
	SNil -> return Nothing

printCallStack :: IO ()
printCallStack = readIORef callstack >>= putStrLn . showCallStack

-- puts a new value to the stack
{-# INLINE pushStack #-}
pushStack :: StackElement inc r m -> IO ()
pushStack = \x -> modifyIORef' callstack (\xs -> {-debug ("pushStack: " ++ showCallStack (SCons x xs)) $-} SCons x xs)

-- removes the value from the stack
{-# INLINE popStack #-}
popStack :: IO (StackElement inc r m)
popStack = atomicModifyIORef' callstack (\(SCons x xs) -> {-debug ("popStack: " ++ showCallStack xs) $-} (xs,x))

-- return the top-most thunk in the stack
{-# INLINE topThunkStack #-}
topThunkStack :: IO (Maybe (StackElement inc r m))
topThunkStack = liftM topStackThunkElement $ readIORef callstack

-- * Adapton layers

instance (MonadRef r m,WeakRef r) => Incremental Adapton r m where
	
	newtype Outside Adapton r m a = Outer { runOuter :: m a } deriving (Functor,Applicative,Monad,MonadIO,MonadRef r,MonadLazy) 
	newtype Inside Adapton r m a = Inner { runInner :: m a } deriving (Functor,Applicative,Monad,MonadIO,MonadRef r,MonadLazy)
	
	world = Outer . runInner
	{-# INLINE world #-}
	unsafeWorld = Inner . runOuter
	{-# INLINE unsafeWorld #-}
	
--	data IncrementalArgs Adapton = AdaptonArgs
	
--	runIncremental _ (Outer m) = m
	runIncremental = runOuter
	{-# INLINE runIncremental #-}

instance MonadTrans (Outside Adapton r) where
	lift = Outer
	{-# INLINE lift #-}
instance MonadTrans (Inside Adapton r) where
	lift = Inner
	{-# INLINE lift #-}
instance InLayer Outside Adapton r m where
	inL = Outer
	{-# INLINE inL #-}
instance InLayer Inside Adapton r m where
	inL = Inner
	{-# INLINE inL #-}


proxyAdapton :: Proxy Adapton
proxyAdapton = Proxy

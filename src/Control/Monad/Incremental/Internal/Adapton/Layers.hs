{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TypeOperators, DeriveDataTypeable, EmptyDataDecls, KindSignatures, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, GeneralizedNewtypeDeriving #-}

module Control.Monad.Incremental.Internal.Adapton.Layers (
	  module Control.Monad.Incremental
	, Inner,Outer
	, topStackThunkElement
	, CallStack, StackElement
	, showCallStack, isThunkStackElement, callstack, topStack, pushStack, popStack, popStack', topThunkStack
	, adaptonParams, proxyAdapton
	, IncParams(..)
	) where

import Control.Monad.Incremental
import System.Mem.Weak

import Control.Monad.Trans.Class
import Data.IORef
import Data.Maybe
import System.IO.Unsafe
import Safe
import Control.Monad.Incremental.Internal.Adapton.Types
import Control.Monad.IO.Class
import Data.Typeable


import Control.Monad.Trans
import Data.Strict.Tuple
import Data.Strict.List as Strict
import Control.Monad

import Data.DeriveTH               
import Data.DeepTypeable
import Data.WithClass.Derive.DeepTypeable
import Language.Haskell.TH.Syntax hiding (lift,Infix,Fixity)
import Data.Strict.Maybe as Strict
import Control.Applicative
import Data.Hashable
import Data.Derive.Memo
import Data.Global.TH as TH

import Debug

$( derive makeDeepTypeableAbstract ''Adapton )

type instance IncK Adapton a = (Typeable a,Eq a)

type Inner = Inside Adapton
type Outer = Outside Adapton

-- * Callstack used by the @Inner@ and @Outer@ monads

topStackThunkElement :: CallStack -> Maybe (StackElement)
topStackThunkElement (SCons x xs) = if isThunkStackElement x then Just x else topStackThunkElement xs
topStackThunkElement SNil = Nothing

-- make sure that the stack is strict, to fix a memory leak with lazy stacks
type CallStack = SList (StackElement)
type StackElement = (NodeMeta :!: SMaybe (IORef (Dependencies))) -- the @Bool@ denotes the kind of the element: true=thunk, false=ref

showCallStack :: CallStack -> String
showCallStack = show . Strict.map (\(x :!: _) -> x)

{-# INLINE isThunkStackElement #-}
isThunkStackElement :: StackElement -> Bool
isThunkStackElement (_ :!: (SJust _)) = True
isThunkStackElement (_ :!: SNothing) = False

TH.declareIORef "callstack"  [t| CallStack |] [e| SNil |]

{-# INLINE topStack #-}
topStack :: IO (Maybe (StackElement))
topStack = readIORef callstack >>= \s -> case s of
	SCons x xs -> return $ Just x
	SNil -> return Nothing

-- puts a new value to the stack
{-# INLINE pushStack #-}
pushStack :: StackElement -> IO ()
pushStack = \x -> modifyIORef' callstack (\xs -> {-debug ("pushStack: " ++ showCallStack (SCons x xs)) $-} SCons x xs)

-- removes the value from the stack
{-# INLINE popStack #-}
popStack :: IO (StackElement)
popStack = atomicModifyIORef' callstack (\(SCons x xs) -> {-debug ("popStack: " ++ showCallStack xs) $-} (xs,x))

{-# INLINE popStack' #-}
popStack' :: (Layer l inc) => l inc (StackElement)
popStack' = unsafeIOToInc $ popStack

-- return the top-most thunk in the stack
{-# INLINE topThunkStack #-}
topThunkStack :: IO (Maybe (StackElement))
topThunkStack = liftM topStackThunkElement $ readIORef callstack

-- * Adapton layers

TH.declareIORef "adaptonParams" [t| IncParams Adapton |] [e| defaultIncParams |]

instance Incremental Adapton where
	
	newtype Outside Adapton a = Outer { runOuter :: IO a } deriving (Functor,Applicative,Monad) 
	newtype Inside Adapton a = Inner { runInner :: IO a } deriving (Functor,Applicative,Monad)
	
	world = Outer . runInner
	{-# INLINE world #-}
	unsafeWorld = Inner . runOuter
	{-# INLINE unsafeWorld #-}
	
	data IncParams Adapton = AdaptonParams { adaptonMemoSize :: Int, adaptonMemoPolicy :: MemoPolicy }
	defaultIncParams = AdaptonParams { adaptonMemoSize = 10^3, adaptonMemoPolicy = MemoLinear }
	
	runIncremental = runOuter . outside
	{-# INLINE runIncremental #-}
	runIncrementalWithParams params m = (writeIORef adaptonParams $! params) >> runOuter (outside m)
	{-# INLINE runIncrementalWithParams #-}
	
	unsafeIOToInc = inside . Inner
	{-# INLINE unsafeIOToInc #-}

proxyAdapton :: Proxy Adapton
proxyAdapton = Proxy

{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TypeOperators, DeriveDataTypeable, EmptyDataDecls, KindSignatures, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, GeneralizedNewtypeDeriving #-}

module Control.Monad.Incremental.Internal.Adapton.Layers (
	  module Control.Monad.Incremental
	, Inner,Outer
	, topStackThunkElement
	, CallStack, StackElement
	, showCallStack, isThunkStackElement, callstack, topStack, pushStack, popStack, popStack', topThunkStack
	, adaptonParams, proxyAdapton
	, IncParams(..)
	, Outside(..)
	, Inside(..)
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
import qualified Data.Strict.List as Strict
import Control.Monad

import Data.DeriveTH               
import Data.DeepTypeable
import Data.WithClass.Derive.DeepTypeable
import Language.Haskell.TH.Syntax hiding (lift,Infix,Fixity)
import qualified Data.Strict.Maybe as Strict
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

topStackThunkElement :: CallStack inc -> Maybe (StackElement inc)
topStackThunkElement (Strict.Cons x xs) = if isThunkStackElement x then Just x else topStackThunkElement xs
topStackThunkElement Strict.Nil = Nothing

showCallStack :: CallStack inc -> String
showCallStack = show . Strict.map (\(x :!: _) -> x)

{-# INLINE isThunkStackElement #-}
isThunkStackElement :: StackElement inc -> Bool
isThunkStackElement (_ :!: (Strict.Just _)) = True
isThunkStackElement (_ :!: Strict.Nothing) = False

TH.declareIORef "callstackAdapton"  [t| CallStack Adapton |] [e| Strict.Nil |]

instance AdaptonImpl Adapton where
	callstack = callstackAdapton

{-# INLINE topStack #-}
topStack :: AdaptonImpl inc => IO (Maybe (StackElement inc))
topStack = readIORef callstack >>= \s -> case s of
	Strict.Cons x xs -> return $ Just x
	Strict.Nil -> return Nothing

-- puts a new value to the stack
{-# INLINE pushStack #-}
pushStack :: AdaptonImpl inc => StackElement inc -> IO ()
pushStack = \x -> modifyIORef' callstack (\xs -> {-debug ("pushStack: " ++ showCallStack (Strict.Cons x xs)) $-} Strict.Cons x xs)

-- removes the value from the stack
{-# INLINE popStack #-}
popStack :: AdaptonImpl inc => IO (StackElement inc)
popStack = atomicModifyIORef' callstack (\(Strict.Cons x xs) -> {-debug ("popStack: " ++ showCallStack xs) $-} (xs,x))

{-# INLINE popStack' #-}
popStack' :: (AdaptonImpl inc,Layer l inc) => l inc (StackElement inc)
popStack' = unsafeIOToInc $ popStack

-- return the top-most thunk in the stack
{-# INLINE topThunkStack #-}
topThunkStack :: AdaptonImpl inc => IO (Maybe (StackElement inc))
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

{-# LANGUAGE ConstraintKinds, DeriveDataTypeable, TemplateHaskell, OverlappingInstances, UndecidableInstances, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Control.Monad.Transactional.TxAdapton.Draw where

import Control.Monad.Transactional
import Control.Monad.Transactional.TxAdapton.Types
import Control.Monad.Transactional.TxAdapton.Layers
import Control.Monad.Transactional.TxAdapton.Algorithm
import Control.Monad.Incremental
import Control.Monad.Incremental.Draw
import Control.Monad.Incremental.Adapton.Algorithm
import Control.Monad.Incremental.Adapton.Layers
import Control.Monad.Incremental.Adapton.Types hiding (MData)
import Control.Monad.Ref
import System.Mem.WeakSet as WeakSet
import Control.Monad.Trans

import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import Data.UUID
import Data.UUID.V1
import Control.Concurrent
import qualified Data.Sequence as Seq
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Data.GraphViz.Types
import Data.GraphViz.Types.Generalised
import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Commands hiding (addExtension)
import Data.Maybe
import Data.Unique
import Data.WithClass.Derive.MData
import Data.DeriveTH
import System.Mem.Weak

import System.Process
import Data.IORef
import System.FilePath.Posix
import System.IO
import System.Directory
import System.IO.Unsafe
import System.Mem
import System.Mem.WeakKey

import Data.WithClass.MData

import Data.IntMap (IntMap(..))
import qualified Data.IntMap as IntMap
import Data.List as List
import qualified Data.Strict.List as SList
import qualified Data.Foldable as Foldable

import Debug

drawTxAdaptonProxy :: Proxy r -> Proxy m -> Proxy (DrawDict TxAdapton r m)
drawTxAdaptonProxy r m = Proxy

instance (MonadRef r m,MonadIO m,Eq a,TxLayer Inside r m,TxLayer Outside r m,TxLayer l r m,MData (DrawDict TxAdapton r m) (Outside TxAdapton r m) a,Input TxM l TxAdapton r m) => Draw TxAdapton r m (TxM l TxAdapton r m a) where
	draw inc r m t = do
		let thunkID = show $ hashUnique $ idTxNM $ metaTxM t
		let thunkNode = mNode thunkID
		(childrenIDs,childrenDot) <- drawDict dict inc r m =<< getOutside t
		let childrenEdges = map (DE . constructorEdge thunkID) childrenIDs
		let childrenRank = sameRank childrenIDs
		dependents <- readTxLog >>= \txlog -> inL $ getTxDependents txlog (metaTxM t) Read
		dependendentEdges <- drawTxDependents thunkID dependents
		return ([thunkID],DN thunkNode : childrenEdges ++ dependendentEdges ++ SG childrenRank : childrenDot)

-- we do not draw any dependencies from thunks, since we assume that they have all already been drawn by following the dependents of modifiables
instance (MonadRef r m,MonadIO m,Eq a,TxLayer Outside r m,TxLayer Inside r m,TxLayer l r m,Output TxU l TxAdapton r m,MData (DrawDict TxAdapton r m) (Outside TxAdapton r m) a) => Draw TxAdapton r m (TxU l TxAdapton r m a) where
	draw inc r m t = do
		let thunkID = show $ hashUnique $ idTxNM $ metaTxU t
		isDirtyUnevaluated <- isDirtyUnevaluatedTxU t
		dependents <- readTxLog >>= \txlog -> inL $ getTxDependents txlog (metaTxU t) Read
		dependentEdges <- drawTxDependents thunkID dependents
		let thunkNode = uNode isDirtyUnevaluated thunkID
		case isDirtyUnevaluated of
			Just _ -> do
				(childrenIDs,childrenDot) <- drawDict dict inc r m =<< inside (oldvalueTxU t)
				let childrenEdges = map (DE . constructorEdge thunkID) childrenIDs
				let childrenRank = sameRank childrenIDs
				return ([thunkID],DN thunkNode : childrenEdges ++ dependentEdges ++ SG childrenRank : childrenDot)
			Nothing -> return ([thunkID],[DN thunkNode])

drawTxDependents :: (TxLayer Outside r m,MonadRef r m,MonadIO m) => String -> TxDependents r m -> Outside TxAdapton r m [DotStatement String]
drawTxDependents fromID deps = checkDrawnDependents fromID $ liftM (concat . Foldable.toList) $ WeakSet.mapM'' (inL . liftIO) (drawTxDependent fromID) deps
drawTxDependent :: (TxLayer Outside r m,MonadRef r m,MonadIO m) => String -> Weak (TxDependent r m) -> Outside TxAdapton r m [DotStatement String]
drawTxDependent fromID weak = do
	mb <- inL $ liftIO $ deRefWeak weak
	case mb of
		Just (TxDependency (srcMetaW,dirtyW,checkW,tgtMetaW,oriW,_)) -> do
			isOriginal <- inL $ readRef oriW
			let ithunkID = show $ hashUnique $ idTxNM tgtMetaW
			let ithunkNode = iuNode ithunkID
			dependents <- readTxLog >>= \txlog -> inL $ getTxDependents txlog tgtMetaW Read
			edges <- drawTxDependents ithunkID dependents
			isDirty <- inL $ readRef dirtyW
			return $ DN ithunkNode : DE (dependentTxEdge isOriginal isDirty fromID ithunkID) : edges
		Nothing -> 	do
			let deadNodeID = "DEAD_WEAK "
			return [DN (deadNode deadNodeID),DE (dependentTxEdge False False fromID deadNodeID)]

dependentTxEdge :: Bool -> Bool -> String -> String -> DotEdge String
dependentTxEdge isOriginal isDirty fromNode toNode = DotEdge {fromNode = fromNode, toNode = toNode, edgeAttributes = [if isOriginal then penWidth 3 else penWidth 1,Color [WC {wColor = X11Color color, weighting = Nothing}],ArrowHead (AType [(ArrMod {arrowFill = FilledArrow, arrowSide = LeftSide},Normal)])]}
	where color = if isDirty then Red else Black

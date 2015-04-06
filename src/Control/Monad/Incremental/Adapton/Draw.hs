{-# LANGUAGE ConstraintKinds, DeriveDataTypeable, TemplateHaskell, OverlappingInstances, UndecidableInstances, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Control.Monad.Incremental.Adapton.Draw where

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

import qualified Data.Map as Map
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
import qualified Data.Foldable as Foldable
import qualified Data.Strict.List as SList

import Debug

drawAdaptonProxy :: Proxy r -> Proxy m -> Proxy (DrawDict Adapton r m)
drawAdaptonProxy r m = Proxy

instance (MonadRef r m,MonadIO m,IncK inc a,Layer l inc r m,MData (DrawDict inc r m) (Outside inc r m) a,Input M l inc r m) => Draw inc r m (M l inc r m a) where
	draw inc r m t = do
		let thunkID = show $ hashUnique $ idNM $ metaM t
		let thunkNode = mNode thunkID ""
		(childrenIDs,childrenDot,childrenTable) <- drawDict dict inc r m =<< getOutside t
		let childrenEdges = map (DE . constructorEdge thunkID) childrenIDs
		let childrenRank = sameRank childrenIDs
		(dependendentEdges,dependentTable) <- drawDependents thunkID (dependentsNM $ metaM t)
		return ([thunkID],DN thunkNode : childrenEdges ++ dependendentEdges ++ SG childrenRank : childrenDot,childrenTable `Map.union` dependentTable)

instance (Input L l inc r m,MonadRef r m,MonadIO m,IncK inc a,Layer l inc r m,MData (DrawDict inc r m) (Outside inc r m) a) => Draw inc r m (L l inc r m a) where
	draw inc r m t = do
		let thunkID = show $ hashUnique $ idNM $ metaL t
		isUnevaluated <- isUnevaluatedL t
		(dependentEdges,dependentTable) <- drawDependents thunkID (dependentsNM $ metaL t)
		let thunkNode = lNode isUnevaluated thunkID
		if isUnevaluated
			then return ([thunkID],DN thunkNode : dependentEdges,dependentTable)
			else do
				(childrenIDs,childrenDot,childrenTable) <- drawDict dict inc r m =<< getOutside t
				let childrenEdges = map (DE . constructorEdge thunkID) childrenIDs
				let childrenRank = sameRank childrenIDs
				return ([thunkID],DN thunkNode : childrenEdges ++ dependentEdges ++ SG childrenRank : childrenDot,childrenTable `Map.union` dependentTable)

-- we do not draw any dependencies from thunks, since we assume that they have all already been drawn by following the dependents of modifiables
instance (MonadRef r m,MonadIO m,IncK inc a,Layer l inc r m,Output U l inc r m,MData (DrawDict inc r m) (Outside inc r m) a) => Draw inc r m (U l inc r m a) where
	draw inc r m t = do
		let thunkID = show $ hashUnique $ idNM $ metaU t
		isDirtyUnevaluated <- isDirtyUnevaluatedU t
		(dependentEdges,dependentTable) <- drawDependents thunkID (dependentsNM $ metaU t)
		(dependencyEdges,dependencyTable) <- drawDependencies thunkID (metaU t)
		let thunkNode = uNode isDirtyUnevaluated thunkID ""
		case isDirtyUnevaluated of
			Just False -> do
				(childrenIDs,childrenDot,childrenTable) <- drawDict dict inc r m =<< inside (oldvalueU t)
				let childrenEdges = map (DE . constructorEdge thunkID) childrenIDs
				let childrenRank = sameRank childrenIDs
				return ([thunkID],DN thunkNode : childrenEdges ++ dependentEdges ++ dependencyEdges ++ SG childrenRank : childrenDot,childrenTable `Map.union` dependentTable `Map.union` dependencyTable)
			Just True -> do
				(childrenIDs,childrenDot,childrenTable) <- drawDict dict inc r m =<< inside (oldvalueU t)
				let childrenEdges = map (DE . constructorEdge thunkID) childrenIDs
				let childrenRank = sameRank childrenIDs
				return ([thunkID],DN thunkNode : childrenEdges ++ dependentEdges ++ dependencyEdges ++ SG childrenRank : childrenDot,childrenTable `Map.union` dependentTable `Map.union` dependencyTable)
			Nothing -> return ([thunkID],[DN thunkNode],Map.empty)

drawDependents :: (Layer Outside inc r m,MonadRef r m,MonadIO m) => String -> Dependents inc r m -> Outside inc r m ([DotStatement String],Map.Map String String)
drawDependents fromID deps = checkDrawnDependents fromID $ liftM (concatOut . Foldable.toList) $ WeakSet.mapM'' (inL . liftIO) (drawDependent fromID) deps
drawDependent :: (Layer Outside inc r m,MonadRef r m,MonadIO m) => String -> Weak (Dependent inc r m) -> Outside inc r m ([DotStatement String],Map.Map String String)
drawDependent fromID weak = do
	mb <- inL $ liftIO $ deRefWeak weak
	case mb of
		Just (Dependency (srcMetaW,dirtyW,checkW,tgtMetaW)) -> do
			let ithunkID = show $ hashUnique $ idNM tgtMetaW
			let ithunkNode = iuNode ithunkID ""
			(edges,edgesTable) <- drawDependents ithunkID (dependentsNM tgtMetaW)
			(edges',edgesTable') <- drawDependencies ithunkID tgtMetaW -- recursive dependencies
			isDirty <- inL $ readRef dirtyW
			return (DN ithunkNode : DE (dependentEdge isDirty fromID ithunkID) : edges ++ edges',edgesTable `Map.union` edgesTable')
		Nothing -> 	do
			let deadNodeID = "DEAD_WEAK "
			return ([DN (deadNode deadNodeID),DE (dependentEdge False fromID deadNodeID)],Map.empty)

concatOut = (\xs -> let (stmts,tbls) = unzip xs in (concat stmts,foldr Map.union Map.empty tbls))

drawDependencies :: (Layer Outside inc r m,MonadRef r m,MonadIO m) => String -> NodeMeta inc r m -> Outside inc r m ([DotStatement String],Map.Map String String)
drawDependencies toID meta = return ([],Map.empty) --checkDrawnDependencies toID $ do
--	mb <- inL $ applyUDataOp (uDataOpUM meta) Nothing (liftM Just . getDependencies) -- recursive dependents
--	case mb of
--		Nothing -> return []
--		Just deps -> drawDependencies' toID deps
drawDependencies' :: (Layer Outside inc r m,MonadRef r m,MonadIO m) => String -> Dependencies inc r m -> Outside inc r m ([DotStatement String],Map.Map String String)
drawDependencies' toID dependencies = liftM concatOut $ mapM (drawDependency toID) dependencies
drawDependency :: (Layer Outside inc r m,MonadRef r m,MonadIO m) => String -> (Dependency inc r m,IO ()) -> Outside inc r m ([DotStatement String],Map.Map String String)
drawDependency toID ((Dependency (srcMeta,dirty,check,tgtMeta)),_) = do
	let ithunkID = show $ hashUnique $ idNM srcMeta
	isDirty <- inL $ readRef dirty
	(edges,edgesTable) <- drawDependents ithunkID (dependentsNM srcMeta)
	(edges',edgesTable') <- drawDependencies ithunkID srcMeta -- recursive dependencies
	return (DE (dependencyEdge isDirty toID ithunkID) : edges ++ edges',edgesTable `Map.union` edgesTable')

getDependencies :: MonadRef r m => r (UData l inc r m a) -> m (Dependencies inc r m)
getDependencies dta = do
	d <- readRef dta
	case d of
		Value dirty value force dependencies -> readRef dependencies
		otherwise -> return []	

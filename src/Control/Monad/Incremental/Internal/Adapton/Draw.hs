{-# LANGUAGE ConstraintKinds, DeriveDataTypeable, TemplateHaskell, OverlappingInstances, UndecidableInstances, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Control.Monad.Incremental.Internal.Adapton.Draw where

import Control.Monad.Incremental
import Control.Monad.Incremental.Draw
import Control.Monad.Incremental.Internal.Adapton.Algorithm
import Control.Monad.Incremental.Internal.Adapton.Layers
import Control.Monad.Incremental.Internal.Adapton.Types hiding (MData)

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
import qualified System.Mem.WeakMap as WeakMap


import Data.WithClass.MData

import Data.IntMap (IntMap(..))
import qualified Data.IntMap as IntMap
import Data.List as List
import qualified Data.Foldable as Foldable
import qualified Data.Strict.List as SList

import Debug

drawAdaptonProxy :: Proxy (DrawDict Adapton)
drawAdaptonProxy = Proxy

instance (IncK inc a,Layer l inc,MData (DrawDict inc) (Outside inc) a,Input M l inc) => Draw inc (M l inc a) where
	draw inc t = do
		let thunkID = show $ hashUnique $ idNM $ metaM t
		let thunkNode = mNode thunkID ""
		(childrenIDs,childrenDot,childrenTable) <- drawDict dict inc =<< getOutside t
		let childrenEdges = map (DE . constructorEdge thunkID) childrenIDs
		let childrenRank = sameRank childrenIDs
		(dependendentEdges,dependentTable) <- unsafeIOToInc $ drawDependents thunkID (dependentsNM $ metaM t)
		return ([thunkID],DN thunkNode : childrenEdges ++ dependendentEdges ++ SG childrenRank : childrenDot,childrenTable `Map.union` dependentTable)

instance (Input L l inc,IncK inc a,Layer l inc,MData (DrawDict inc) (Outside inc) a) => Draw inc (L l inc a) where
	draw inc t = do
		let thunkID = show $ hashUnique $ idNM $ metaL t
		isUnevaluated <- isUnevaluatedL t
		(dependentEdges,dependentTable) <- unsafeIOToInc $ drawDependents thunkID (dependentsNM $ metaL t)
		let thunkNode = lNode isUnevaluated thunkID
		if isUnevaluated
			then return ([thunkID],DN thunkNode : dependentEdges,dependentTable)
			else do
				(childrenIDs,childrenDot,childrenTable) <- drawDict dict inc =<< getOutside t
				let childrenEdges = map (DE . constructorEdge thunkID) childrenIDs
				let childrenRank = sameRank childrenIDs
				return ([thunkID],DN thunkNode : childrenEdges ++ dependentEdges ++ SG childrenRank : childrenDot,childrenTable `Map.union` dependentTable)

-- we do not draw any dependencies from thunks, since we assume that they have all already been drawn by following the dependents of modifiables
instance (IncK inc a,Layer l inc,Output U l inc,MData (DrawDict inc) (Outside inc) a) => Draw inc (U l inc a) where
	draw inc t = do
		let thunkID = show $ hashUnique $ idNM $ metaU t
		isDirtyUnevaluated <- isDirtyUnevaluatedU t
		(dependentEdges,dependentTable) <- unsafeIOToInc $ drawDependents thunkID (dependentsNM $ metaU t)
		(dependencyEdges,dependencyTable) <- unsafeIOToInc $ drawDependencies thunkID (metaU t)
		let thunkNode = uNode isDirtyUnevaluated thunkID ""
		case isDirtyUnevaluated of
			Just False -> do
				(childrenIDs,childrenDot,childrenTable) <- drawDict dict inc =<< inside (oldvalueU t)
				let childrenEdges = map (DE . constructorEdge thunkID) childrenIDs
				let childrenRank = sameRank childrenIDs
				return ([thunkID],DN thunkNode : childrenEdges ++ dependentEdges ++ dependencyEdges ++ SG childrenRank : childrenDot,childrenTable `Map.union` dependentTable `Map.union` dependencyTable)
			Just True -> do
				(childrenIDs,childrenDot,childrenTable) <- drawDict dict inc =<< inside (oldvalueU t)
				let childrenEdges = map (DE . constructorEdge thunkID) childrenIDs
				let childrenRank = sameRank childrenIDs
				return ([thunkID],DN thunkNode : childrenEdges ++ dependentEdges ++ dependencyEdges ++ SG childrenRank : childrenDot,childrenTable `Map.union` dependentTable `Map.union` dependencyTable)
			Nothing -> return ([thunkID],[DN thunkNode],Map.empty)

drawDependents :: String -> Dependents -> IO ([DotStatement String],Map.Map String String)
drawDependents fromID deps = checkDrawnDependents fromID $ WeakMap.foldWeakM (drawDependent fromID) ([],Map.empty) deps
drawDependent :: String -> ([DotStatement String],Map.Map String String) -> (Unique,Weak Dependent) -> IO ([DotStatement String],Map.Map String String)
drawDependent fromID (stmts,html) (_,weak) = do
	mb <- deRefWeak weak
	case mb of
		Just (Dependency (srcMetaW,dirtyW,checkW,tgtMetaW)) -> do
			let ithunkID = show $ hashUnique $ idNM tgtMetaW
			let ithunkNode = iuNode ithunkID ""
			(edges,edgesTable) <- drawDependents ithunkID (dependentsNM tgtMetaW)
			(edges',edgesTable') <- drawDependencies ithunkID tgtMetaW -- recursive dependencies
			isDirty <- readIORef dirtyW
			return (DN ithunkNode : DE (dependentEdge isDirty fromID ithunkID) : edges ++ edges' ++ stmts,edgesTable `Map.union` edgesTable' `Map.union` html)
		Nothing -> 	do
			let deadNodeID = "DEAD_WEAK "
			return (DN (deadNode deadNodeID) : DE (dependentEdge False fromID deadNodeID) : stmts,html)

concatOut xs = (concat stmts,foldr Map.union Map.empty tbls)
	where (stmts,tbls) = unzip xs

drawDependencies :: String -> NodeMeta -> IO ([DotStatement String],Map.Map String String)
drawDependencies toID meta = return ([],Map.empty) --checkDrawnDependencies toID $ do
--	mb <- liftIO $ applyUDataOp (uDataOpUM meta) Nothing (liftM Just . getDependencies) -- recursive dependents
--	case mb of
--		Nothing -> return []
--		Just deps -> drawDependencies' toID deps
drawDependencies' :: String -> Dependencies -> IO ([DotStatement String],Map.Map String String)
drawDependencies' toID dependencies = liftM concatOut $ mapM (drawDependency toID) dependencies
drawDependency :: String -> (Dependency,IO ()) -> IO ([DotStatement String],Map.Map String String)
drawDependency toID ((Dependency (srcMeta,dirty,check,tgtMeta)),_) = do
	let ithunkID = show $ hashUnique $ idNM srcMeta
	isDirty <- readIORef dirty
	(edges,edgesTable) <- drawDependents ithunkID (dependentsNM srcMeta)
	(edges',edgesTable') <- drawDependencies ithunkID srcMeta -- recursive dependencies
	return (DE (dependencyEdge isDirty toID ithunkID) : edges ++ edges',edgesTable `Map.union` edgesTable')

getDependencies :: IORef (UData l inc a) -> IO Dependencies
getDependencies dta = do
	d <- readIORef dta
	case d of
		Value dirty value force dependencies -> readIORef dependencies
		otherwise -> return []	

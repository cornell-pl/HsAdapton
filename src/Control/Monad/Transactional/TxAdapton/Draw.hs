{-# LANGUAGE MagicHash, ConstraintKinds, DeriveDataTypeable, TemplateHaskell, OverlappingInstances, UndecidableInstances, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Control.Monad.Transactional.TxAdapton.Draw where

import Control.Monad.Transactional
import Control.Monad.Transactional.TxAdapton.Types
import Control.Monad.Transactional.TxAdapton.Layers
import Control.Monad.Transactional.TxAdapton.Algorithm
import Control.Monad.Incremental
import Control.Monad.Incremental.Draw
import Control.Monad.Incremental.Adapton.Algorithm
import Control.Monad.Incremental.Adapton.Layers
import Control.Monad.Incremental.Adapton.Draw
import Control.Monad.Incremental.Adapton.Types hiding (MData)
import Control.Monad.Ref
import System.Mem.WeakSet as WeakSet
import Control.Monad.Trans

import Control.Monad
import Control.Monad.IO.Class
import System.Mem.WeakTable as WeakTable
import Data.Strict.Tuple
import System.Mem.WeakMap as WeakMap
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
import qualified Data.Strict.List as SList
import qualified Data.Foldable as Foldable

import Debug

statusColor :: TxStatus -> Color
statusColor status = X11Color $ case status of
	TxStatus (Read,False) -> Black
	TxStatus (Read,True) -> CornFlowerBlue
	TxStatus (Eval,False) -> Blue3
	TxStatus (Eval,True) -> Blue4
	TxStatus (Write,False) -> Firebrick2
	TxStatus (Write,True) -> Firebrick3
	TxStatus (New False,False) -> Green3
	TxStatus (New False,True) -> Green4
	TxStatus (New True,False) -> HotPink3
	TxStatus (New True,True) -> HotPink4

drawTxAdaptonProxy :: Proxy r -> Proxy m -> Proxy (DrawDict TxAdapton r m)
drawTxAdaptonProxy r m = Proxy

instance (MonadRef r m,MonadIO m,IncK TxAdapton a,TxLayer Inside r m,TxLayer Outside r m,TxLayer l r m,MData (DrawDict TxAdapton r m) (Outside TxAdapton r m) a,Input TxM l TxAdapton r m) => Draw TxAdapton r m (TxM l TxAdapton r m a) where
	draw inc r m t = do
		let thunkID = show $ hashUnique $ idTxNM $ metaTxM t
		(childrenIDs,childrenDot,childrenTable) <- drawDict dict inc r m =<< getOutside t
		let childrenEdges = map (DE . constructorEdge thunkID) childrenIDs
		let childrenRank = sameRank childrenIDs
		(dependents,status) <- readTxLog >>= \txlog -> inL $ getTxDependentsStatus txlog (metaTxM t)
		(dependendentEdges,dependentsTable) <- drawTxDependents thunkID dependents
		let thunkNode = addAttribute (FontColor $ statusColor status) $ mNode thunkID ""
		return ([thunkID],DN thunkNode : childrenEdges ++ dependendentEdges ++ SG childrenRank : childrenDot,childrenTable `Map.union` dependentsTable)

-- we do not draw any dependencies from thunks, since we assume that they have all already been drawn by following the dependents of modifiables
instance (MonadRef r m,MonadIO m,IncK TxAdapton a,TxLayer Outside r m,TxLayer Inside r m,TxLayer l r m,Output TxU l TxAdapton r m,MData (DrawDict TxAdapton r m) (Outside TxAdapton r m) a) => Draw TxAdapton r m (TxU l TxAdapton r m a) where
	draw inc r m t = do
		let thunkID = show $ hashUnique $ idTxNM $ metaTxU t
		isDirtyUnevaluated <- isDirtyUnevaluatedTxU t
		(dependents,status) <- readTxLog >>= \txlog -> inL $ getTxDependentsStatus txlog (metaTxU t)
		(dependentEdges,dependentsTable) <- drawTxDependents thunkID dependents
		(thunkDependencies,thunkTable) <- drawTxDependencies $ metaTxU t
		let thunkNode = addAttribute (FontColor $ statusColor status) $ uNode isDirtyUnevaluated thunkID ('_':thunkDependencies)
		case isDirtyUnevaluated of
			Just _ -> do
				(childrenIDs,childrenDot,childrenTable) <- drawDict dict inc r m =<< inside (oldvalueTxU t)
				let childrenEdges = map (DE . constructorEdge thunkID) childrenIDs
				let childrenRank = sameRank childrenIDs
				return ([thunkID],DN thunkNode : childrenEdges ++ dependentEdges ++ SG childrenRank : childrenDot,thunkTable `Map.union` childrenTable `Map.union` dependentsTable)
			Nothing -> return ([thunkID],[DN thunkNode],thunkTable `Map.union` dependentsTable)

drawTxDependents :: (TxLayer Outside r m,MonadRef r m,MonadIO m) => String -> (TxDependents r m,Maybe (TxDependents r m)) -> Outside TxAdapton r m ([DotStatement String],Map.Map String String)
drawTxDependents fromID (deps,Nothing) = checkDrawnDependents fromID $ liftM concatOut $ WeakMap.mapM'' (inL . liftIO) (drawTxDependent fromID) deps
drawTxDependents fromID (deps,Just txdeps) = checkDrawnDependents fromID $ do
	(edges1,tbl1) <- liftM concatOut $ WeakMap.mapM'' (inL . liftIO) (drawTxDependent fromID) txdeps
	(edges2,tbl2) <- liftM concatOut $ WeakMap.mapM'' (inL . liftIO) (drawTxDependent fromID) deps
	return (edges1 ++ edges2,tbl1 `Map.union` tbl2)
drawTxDependent :: (TxLayer Outside r m,MonadRef r m,MonadIO m) => String -> Weak (TxDependent r m) -> Outside TxAdapton r m ([DotStatement String],Map.Map String String)
drawTxDependent fromID weak = do
	mb <- inL $ liftIO $ deRefWeak weak
	case mb of
		Just (TxDependency (srcMetaW,dirtyW,checkW,tgtMetaW,oriW,_)) -> do
			isOriginal <- inL $ readRef oriW
			(ithunkDependencies,ithunkTable) <- drawTxDependencies tgtMetaW
			let ithunkID = show $ hashUnique $ idTxNM tgtMetaW
			(dependents,status) <- readTxLog >>= \txlog -> inL $ getTxDependentsStatus txlog tgtMetaW
			(edges,edgesTable) <- drawTxDependents ithunkID dependents
			isDirty <- inL $ readRef dirtyW
			let ithunkNode = addAttribute (FontColor $ statusColor status) $ iuNode ithunkID ('_':ithunkDependencies)
			return (DN ithunkNode : DE (dependentTxEdge isOriginal isDirty fromID ithunkID) : edges,ithunkTable `Map.union` edgesTable)
		Nothing -> 	do
			let deadNodeID = "DEAD_WEAK "
			return ([DN (deadNode deadNodeID),DE (dependentTxEdge False False fromID deadNodeID)],Map.empty)

dependentTxEdge :: Bool -> Bool -> String -> String -> DotEdge String
dependentTxEdge isOriginal isDirty fromNode toNode = DotEdge {fromNode = fromNode, toNode = toNode, edgeAttributes = [if isOriginal then penWidth 3 else penWidth 1,Color [WC {wColor = X11Color color, weighting = Nothing}],ArrowHead (AType [(ArrMod {arrowFill = FilledArrow, arrowSide = LeftSide},Normal)])]}
	where color = if isDirty then Red else Black

drawTxDependencies :: (TxLayer Outside r m,MonadRef r m,MonadIO m) => TxNodeMeta r m -> Outside TxAdapton r m (String,Map.Map String String)
drawTxDependencies meta = do
	txlogs <- readTxLog
	mb <- inL $ liftIO $ findTxLogEntry txlogs (idTxNM meta)
	case mb of
		Nothing -> return ("?",Map.empty)
		Just tvar -> do
			
			-- draw value
			strValue <- drawDynTxVar tvar
			
			-- get dependencies
			mb <- inL $ dynTxVarDependencies tvar
			strDependencies <- case mb of
				Nothing -> return ""
				Just deps -> do
					xs <- readRef deps
					let conc str (d,_) = do
						isDirty <- readRef (dirtyTxW d)
						return $ str ++","++ show (idTxNM $ srcMetaTxW d) ++ (if isDirty then "*" else "")
					Foldable.foldlM conc "" xs
			return (strDependencies,Map.singleton (show $ idTxNM meta) strValue)

getTxDependentsStatus :: MonadIO m => TxLogs r m -> TxNodeMeta r m -> m ((TxDependents r m,Maybe (TxDependents r m)),TxStatus)
getTxDependentsStatus txlogs meta = do		
	mb <- liftIO $ findTxLogEntry txlogs (idTxNM meta)
	case mb of
		Just (DynTxU _ mbtxdeps _ status) -> return ((dependentsTxNM meta,mbtxdeps),status)
		Just (DynTxM _ mbtxdeps _ status) -> return ((dependentsTxNM meta,mbtxdeps),status)
		otherwise -> return ((dependentsTxNM meta,Nothing),TxStatus (Read,False))

drawDynTxVar :: DynTxVar r m -> Outside TxAdapton r m String
drawDynTxVar (DynTxU (Just (BuffTxU buff_dta)) _ u _) = do
	(dta,_) <- inL $ readRef buff_dta
	case dta of
		TxValue 0# v _ _ -> inside $ showIncK v
		TxValue 1# v _ _ -> liftM (++"*") $ inside $ showIncK v
		TxConst v -> inside $ showIncK v
		otherwise -> return ""
drawDynTxVar (DynTxU Nothing _ u _) = do
	dta <- inL $ readRef $ dataTxU u
	case dta of
		TxValue 0# v _ _ -> inside $ showIncK v
		TxValue 1# v _ _ -> liftM (++"*") $ inside $ showIncK v
		TxConst v -> inside $ showIncK v
		otherwise -> return ""
drawDynTxVar (DynTxM (Just (BuffTxM v)) _ m _) = do
	inside $ showIncK v
drawDynTxVar (DynTxM Nothing _ m _) = do
	v <- inL $ readRef (dataTxM m)
	inside $ showIncK v

--getTxDependenciesStatus :: (MonadIO m,MonadRef r m) => TxLogs r m -> TxNodeMeta r m -> TxStatus -> m (TxDependencies r m,TxStatus)
--getTxDependenciesStatus tbl meta status = do
--	var <- bufferTxNM meta status tbl
--	Just deps <- dynTxVarDependencies var
--	return (deps,dynTxStatus var)

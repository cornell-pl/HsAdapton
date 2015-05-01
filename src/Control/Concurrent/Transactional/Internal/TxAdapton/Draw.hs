{-# LANGUAGE DoAndIfThenElse, DataKinds, ScopedTypeVariables, BangPatterns, ViewPatterns, MagicHash, ConstraintKinds, DeriveDataTypeable, TemplateHaskell, OverlappingInstances, UndecidableInstances, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Control.Concurrent.Transactional.Internal.TxAdapton.Draw where

import Control.Concurrent.Transactional
import Control.Concurrent.Transactional.Internal.TxAdapton.Types
import Control.Concurrent.Transactional.Internal.TxAdapton.Layers
import Control.Concurrent.Transactional.Internal.TxAdapton.Algorithm hiding (drawTxU,drawTxM)
import Control.Monad.Incremental
import Control.Monad.Incremental.Draw
import Control.Monad.Incremental.Internal.Adapton.Algorithm
import Control.Monad.Incremental.Internal.Adapton.Layers
import Control.Monad.Incremental.Internal.Adapton.Draw
import Control.Monad.Incremental.Internal.Adapton.Types hiding (MData)

import Control.Monad.Trans

import Control.Monad
import Control.Monad.IO.Class
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
import System.Mem.Weak as Weak

import System.Process
import Data.IORef
import System.FilePath.Posix
import System.IO
import System.Directory
import System.IO.Unsafe
import System.Mem


import Data.WithClass.MData

import Data.IntMap (IntMap(..))
import qualified Data.IntMap as IntMap
import Data.List as List
import qualified Data.Strict.List as SList
import qualified Data.Foldable as Foldable

import Debug

statusColor :: TxStatus -> Color
statusColor status = X11Color $ case status of
	TxStatus (Read Nothing :!: False) -> Black
	TxStatus (Read Nothing :!: True) -> SteelBlue
	TxStatus (Read (Just _) :!: False) -> Aquamarine2
	TxStatus (Read (Just _) :!: True) -> Aquamarine3
	TxStatus (Eval Nothing :!: _) -> Blue4
	TxStatus (Eval (Just _) :!: _) -> DarkViolet
	TxStatus (Write Nothing :!: _) -> Chocolate4
	TxStatus (Write (Just _) :!: _) -> Magenta
	TxStatus (New False :!: _) -> Green4
	TxStatus (New True :!: _) -> Yellow2

drawTxAdaptonProxy :: Proxy (DrawDict (TxAdapton c))
drawTxAdaptonProxy = Proxy

instance (IncK (TxAdapton c) a,TxLayer Inside c,TxLayer Outside c,TxLayer l c,MData (DrawDict (TxAdapton c)) (Outside (TxAdapton c)) a,Input (TxM i c) l (TxAdapton c)) => Draw (TxAdapton c) ((TxM i c) l (TxAdapton c) a) where
	draw inc t = do
		let thunkID = show $ hashUnique $ idTxNM $ metaTxM t
		checkDrawn thunkID $ do
--			debugTx $ "drawing " ++ show thunkID
			start <- readTxId
			(isFuture,_) <- readRootTx
			thread <- unsafeIOToInc myThreadId
			let label = show thread ++" "++ show start
			let txt = DN $ DotNode {nodeID = label, nodeAttributes = [Shape PlainText,Label (StrLabel $ T.pack label)]}
			(childrenIDs,childrenDot,childrenTable) <- drawDict dict inc =<< getOutside t
			let childrenEdges = map (DE . constructorEdge thunkID) childrenIDs
			let childrenRank = sameRank childrenIDs
			(dependents,status) <- readTxLogs >>= \txlog -> unsafeIOToInc $ getTxDependentsStatus isFuture txlog (metaTxM t)
			(dependendentEdges,dependentsTable) <- drawTxDependents thunkID dependents
			let thunkNode = addAttribute (FontColor $ statusColor status) $ mNode thunkID ""
			return ([thunkID],txt : DN thunkNode : childrenEdges ++ dependendentEdges ++ SG childrenRank : childrenDot,childrenTable `Map.union` dependentsTable)

-- we do not draw any dependencies from thunks, since we assume that they have all already been drawn by following the dependents of modifiables
instance (IncK (TxAdapton c) a,TxLayer Outside c,TxLayer Inside c,TxLayer l c,Output (TxU c) l (TxAdapton c),MData (DrawDict (TxAdapton c)) (Outside (TxAdapton c)) a) => Draw (TxAdapton c) ((TxU c) l (TxAdapton c) a) where
	draw inc t = do
		let doNode = False
		let thunkID = show $ hashUnique $ idTxNM $ metaTxU t
		checkDrawn thunkID $ do
--			debugTx $ "drawing " ++ show thunkID
			isDirtyUnevaluated <- isDirtyUnevaluatedTxU t
			(isFuture,_) <- readRootTx
			(dependents,status) <- readTxLogs >>= \txlog -> unsafeIOToInc $ getTxDependentsStatus isFuture txlog (metaTxU t)
			(dependentEdges,dependentsTable) <- drawTxDependents thunkID dependents
			(thunkDependenciesStr,thunkDependencies,thunkTable) <- drawTxDependencies thunkID $ metaTxU t
			let thunkNode = addAttribute (FontColor $ statusColor status) $ uNode isDirtyUnevaluated thunkID ('_':thunkDependenciesStr)
			case isDirtyUnevaluated of
				Just _ -> do
					if doNode
						then do
							(childrenIDs,childrenDot,childrenTable) <- drawDict dict inc =<< inside (oldvalueTxU t)
							let childrenEdges = map (DE . constructorEdge thunkID) childrenIDs
							let childrenRank = sameRank childrenIDs
							return ([thunkID],DN thunkNode : thunkDependencies ++ childrenEdges ++ dependentEdges ++ SG childrenRank : childrenDot,thunkTable `Map.union` childrenTable `Map.union` dependentsTable)
					else return ([thunkID],DN thunkNode : thunkDependencies ++ dependentEdges,thunkTable `Map.union` dependentsTable)
				Nothing -> return ([thunkID],[DN thunkNode] ++ thunkDependencies ++ dependentEdges,thunkTable `Map.union` dependentsTable)


drawTxDependents :: (TxLayer Outside c,TxLayer Inside c) => String -> (TxDependents c,Maybe (TxDependents c)) -> Outside (TxAdapton c) ([DotStatement String],Map.Map String String)
drawTxDependents fromID (deps,Nothing) = checkDrawnDependentsLayer fromID $ WeakMap.foldWeakGenericM unsafeIOToInc (drawTxDependent fromID) ([],Map.empty) deps
drawTxDependents fromID (deps,Just txdeps) = checkDrawnDependentsLayer fromID $ do
	xs <- WeakMap.foldWeakGenericM unsafeIOToInc (drawTxDependent fromID) ([],Map.empty) txdeps
	WeakMap.foldWeakGenericM unsafeIOToInc (drawTxDependent fromID) xs deps
drawTxDependent :: (TxLayer Outside c,TxLayer Inside c) => String -> ([DotStatement String],Map.Map String String) -> (Unique,Weak (TxDependent c)) -> Outside (TxAdapton c) ([DotStatement String],Map.Map String String)
drawTxDependent fromID (stmts,html) (_,weak) = do
	mb <- unsafeIOToInc $ deRefWeak weak
	case mb of
		Just d -> drawTxDependent' fromID (stmts,html) d
		Nothing -> 	do
--			let deadNodeID = "DEAD_WEAK "
--			return (DN (deadNode deadNodeID) : DE (dependentTxEdge False False fromID deadNodeID) : stmts,html)
			return (stmts,html)

drawTxDependent' :: (TxLayer Outside c,TxLayer Inside c) => String -> ([DotStatement String],Map.Map String String) -> (TxDependency c) -> Outside (TxAdapton c) ([DotStatement String],Map.Map String String)
drawTxDependent' fromID (stmts,html) (TxDependency (srcMetaW :!: dirtyW :!: checkW :!: tgtMetaW :!: oriW :!: _)) = do
			let ithunkID = show $ hashUnique $ idTxNM tgtMetaW
			(isFuture,_) <- readRootTx
			isOriginal <- unsafeIOToInc $ readIORef oriW
			(ithunkDependenciesStr,ithunkDependencies,ithunkTable) <- drawTxDependencies ithunkID tgtMetaW
			(dependents,status) <- readTxLogs >>= \txlog -> unsafeIOToInc $ getTxDependentsStatus isFuture txlog tgtMetaW
			(edges,edgesTable) <- drawTxDependents ithunkID dependents
			isDirty <- unsafeIOToInc $ readIORef dirtyW
			let ithunkNode = addAttribute (FontColor $ statusColor status) $ iuNode ithunkID ('_':ithunkDependenciesStr)
			return (DN ithunkNode : DE (dependentTxEdge isOriginal isDirty fromID ithunkID) : ithunkDependencies ++ edges ++ stmts,ithunkTable `Map.union` edgesTable `Map.union` html)

dependentTxEdge :: Bool -> Bool -> String -> String -> DotEdge String
dependentTxEdge isOriginal isDirty fromNode toNode = DotEdge {fromNode = fromNode, toNode = toNode, edgeAttributes = [if isOriginal then penWidth 3 else penWidth 1,Color [WC {wColor = X11Color color, weighting = Nothing}],ArrowHead (AType [(ArrMod {arrowFill = FilledArrow, arrowSide = LeftSide},Normal)])]}
	where color = if isDirty then Red else Black

drawTxDependencies :: (TxLayer Outside c,TxLayer Inside c) => String -> TxNodeMeta c -> Outside (TxAdapton c) (String,[DotStatement String],Map.Map String String)
drawTxDependencies toID meta = do
	txlogs <- readTxLogs
	(isFuture,_) <- readRootTx
	thread <- unsafeIOToInc myThreadId
	mb <- unsafeIOToInc $ findTxContentEntry isFuture thread txlogs meta
	
	let drawTVar tvar = do
		
		-- draw value
		(strValue,stmts,html) <- drawDynTxVar tvar
		
		-- get dependencies
		mb <- unsafeIOToInc $ dynTxVarDependencies tvar
		(strDependencies,stmts',html') <- case mb of
			Nothing -> return ("",stmts,html)
			Just deps -> do
				xs <- unsafeIOToInc $ readIORef deps
				let conc (str,stmts,html) (d,_) = do
					
					(_,stmts',html') <- drawTxNM (srcMetaTxW d)
					
					--mb <- unsafeIOToInc $ findTxContentEntry thread txlogs (srcMetaTxW d)
					--
					--(str,stmts',html') <- case mb of
					--	Nothing -> return html
					--	Just (tvar,_) -> liftM (\x -> Map.insert (show $ idTxNM $ srcMetaTxW d) x html) $ drawDynTxVar tvar
					
					isDirty <- unsafeIOToInc $ readIORef (dirtyTxW d)
					return (str ++","++ show (idTxNM $ srcMetaTxW d) ++ (if isDirty then "*" else ""),stmts ++ stmts',html `Map.union` html')
				Foldable.foldlM conc ("",stmts,html) xs
				
		return (strDependencies,stmts',html' `Map.union` Map.singleton (show $ idTxNM meta) strValue)
	
	case mb of
		Nothing -> do
			(_,stmts,html) <- drawTxNM meta
			return ("?",stmts,html)
		Just (!tvar,!isTop) -> drawTVar tvar

getTxDependentsStatus :: TxLayer Outside c => Bool -> TxLogs c -> TxNodeMeta c -> IO ((TxDependents c,Maybe (TxDependents c)),TxStatus)
getTxDependentsStatus isFuture txlogs meta = do		
	thread <- myThreadId
	mb <- findTxContentEntry isFuture thread txlogs meta
	case mb of
		Just (!(DynTxU _ txdeps _ txstat),!isTop) -> readIORef txstat >>= \stat -> return ((dependentsTxNM meta,Just txdeps),stat)
		Just (!(DynTxM _ txdeps _ txstat),!isTop) -> readIORef txstat >>= \stat -> return ((dependentsTxNM meta,Just txdeps),stat)
		otherwise -> return ((dependentsTxNM meta,Nothing),TxStatus (Read Nothing :!: False))

drawDynTxVar :: (TxLayer Outside c,TxLayer Inside c) => DynTxVar c -> Outside (TxAdapton c) (String,[DotStatement String],Map.Map String String)
drawDynTxVar (DynTxU (BuffTxU buff_dta) _ (u :: TxU c l (TxAdapton c) a) txstat) = do
	let l = Proxy :: Proxy l
	(_,stmts,html) <- case (toLayerKind l) of
		Inside -> let u1 :: TxU c Inside (TxAdapton c) a = coerce u in draw Proxy u1
		Outside -> let u1 :: TxU c Outside (TxAdapton c) a = coerce u in draw Proxy u1
	stat <- unsafeIOToInc $ readIORef txstat
	str <- case stat of
		(isEvalOrWrite -> Just _) -> do
			(dta,_) <- unsafeIOToInc $ readIORef buff_dta
			case dta of
				TxValue 0# v _ _ -> inside $ displayK v
				TxValue 1# v _ _ -> liftM (++"*") $ inside $ displayK v
				TxConst v -> inside $ displayK v
				otherwise -> return ""
		otherwise -> do
			dta <- unsafeIOToInc $ readIORef $ dataTxU u
			case dta of
				TxValue 0# v _ _ -> inside $ displayK v
				TxValue 1# v _ _ -> liftM (++"*") $ inside $ displayK v
				TxConst v -> inside $ displayK v
				otherwise -> return ""
	return (str,stmts,html)
drawDynTxVar (DynTxM (BuffTxM value) _ (m :: TxM i c l (TxAdapton c) a) txstat) = do
	let i = Proxy :: Proxy i
	let l = Proxy :: Proxy l
	(_,stmts,html) <- case (toIsolation i,toLayerKind l) of
		(Versioned,Inside) -> let m1 :: TxM Versioned c Inside (TxAdapton c) a = coerce m in draw Proxy m1
		(Forgetful,Inside) -> let m1 :: TxM Forgetful c Inside (TxAdapton c) a = coerce m in draw Proxy m1
		(Cumulative,Inside) -> let m1 :: TxM Cumulative c Inside (TxAdapton c) a = coerce m in draw Proxy m1
		(Versioned,Outside) -> let m1 :: TxM Versioned c Outside (TxAdapton c) a = coerce m in draw Proxy m1
		(Forgetful,Outside) -> let m1 :: TxM Forgetful c Outside (TxAdapton c) a = coerce m in draw Proxy m1
		(Cumulative,Outside) -> let m1 :: TxM Cumulative c Outside (TxAdapton c) a = coerce m in draw Proxy m1
	stat <- unsafeIOToInc $ readIORef txstat
	str <- case stat of
		(isEvalOrWrite -> Just _) -> do
			!v <- unsafeIOToInc $ readIORef value
			inside $ displayK v
		otherwise -> do
			!v <- unsafeIOToInc $ readIORef (dataTxM m)
			inside $ displayK v
	return (str,stmts,html)
	
	
drawTxU :: (IncK (TxAdapton c) a,TxLayer l c,TxLayer Outside c,TxLayer Inside c) => TxU c l (TxAdapton c) a -> Outside (TxAdapton c) (String,[DotStatement String],Map.Map String String)
drawTxU (u :: TxU c l (TxAdapton c) a) = do
--	let u1 :: TxU c Inside (TxAdapton c) a = coerce u
	(_,stmts,html) <- drawTxNM (metaTxU u)
	str <- do
			dta <- unsafeIOToInc $ readIORef $ dataTxU u
			case dta of
				TxValue 0# v _ _ -> inside $ displayK v
				TxValue 1# v _ _ -> liftM (++"*") $ inside $ displayK v
				TxConst v -> inside $ displayK v
				otherwise -> return ""
	return (str,stmts,html)	

drawTxM :: (IncK (TxAdapton c) a,TxLayer l c,TxLayer Outside c,TxLayer Inside c) => TxM i c l (TxAdapton c) a -> Outside (TxAdapton c) (String,[DotStatement String],Map.Map String String)
drawTxM (m :: TxM i c l (TxAdapton c) a) = do
--	let m1 :: TxM i c Inside (TxAdapton c) a = coerce m
	(_,stmts,html) <- drawTxNM (metaTxM m)
	str <- do
			!v <- unsafeIOToInc $ readIORef (dataTxM m)
			inside $ displayK v
	return (str,stmts,html)

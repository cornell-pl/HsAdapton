{-# LANGUAGE ViewPatterns, ScopedTypeVariables, DeriveDataTypeable, TemplateHaskell, OverlappingInstances, UndecidableInstances, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Control.Monad.Incremental.Draw where

import Control.Monad.Incremental
import Control.Monad.Incremental.Adapton.Algorithm
import Control.Monad.Incremental.Adapton.Layers
import Control.Monad.Incremental.Adapton.Types hiding (MData)
import Control.Monad.Ref
import System.Mem.WeakSet as WeakSet
import Control.Monad.Trans
import System.Mem.WeakRef

import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import Data.UUID
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Set (Set(..))
import qualified Data.Set as Set
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
import Control.Monad.Ref

import Data.WithClass.MData

import Data.IntMap (IntMap(..))
import qualified Data.IntMap as IntMap

import Debug

import Data.DeriveTH                 -- Library for deriving instances for existing types
import Data.DeepTypeable
import Data.WithClass.Derive.DeepTypeable
import Language.Haskell.TH.Syntax

data Merge a b = Merge a b deriving (Typeable,Eq,Show,Ord) -- a special type that allows passing pairs of arguments to @drawToDot@

$( derive makeMData ''Merge )

$( derive makeDeepTypeable ''Merge )

--instance (DeepTypeable a,DeepTypeable b) => DeepTypeable (Merge a b) where
--	typeTree (_::Proxy (Merge a b)) = MkTypeTree (mkName "Control.Monad.Incremental.Draw.Merge") [MkConTree (mkName "Control.Monad.Incremental.Draw.Merge") [typeTree (Proxy :: Proxy a),typeTree (Proxy :: Proxy b)]]

-- hack to automatize drawing sequences

{-# NOINLINE tempPDFs #-}
tempPDFs :: IORef [FilePath]
tempPDFs = unsafePerformIO $ newIORef []

{-# NOINLINE drawnDependents #-}
drawnDependents :: IORef [String]
drawnDependents = unsafePerformIO $ newIORef []

{-# NOINLINE drawnDependencies #-}
drawnDependencies :: IORef [String]
drawnDependencies = unsafePerformIO $ newIORef []

checkDrawnDependencies :: (Layer l inc r m,MonadIO m) => String -> l inc r m [a] -> l inc r m [a]
checkDrawnDependencies node m = do
	nodes <- inL $ liftIO $ readIORef drawnDependencies
	if node `elem` nodes
		then return []
		else inL (liftIO (writeIORef drawnDependencies (node:nodes))) >> m

checkDrawnDependents :: (Layer l inc r m,MonadIO m) => String -> l inc r m [a] -> l inc r m [a]
checkDrawnDependents node m = do
	nodes <- inL $ liftIO $ readIORef drawnDependents
	if node `elem` nodes
		then return []
		else inL (liftIO (writeIORef drawnDependents (node:nodes))) >> m

resetDrawnNodes :: MonadIO m => m ()
resetDrawnNodes = liftIO $ writeIORef drawnDependencies [] >> writeIORef drawnDependents []

-- * Graphviz Drawing classes

-- a list of top-level ids and (elements of) a graph
type DrawDot = ([String],[DotStatement String])

class (MonadRef r m,MonadIO m,Layer Outside inc r m) => Draw inc r m a where
	draw :: Proxy inc -> Proxy r -> Proxy m -> a -> Outside inc r m DrawDot

data DrawDict inc r m a = DrawDict { drawDict :: Proxy inc -> Proxy r -> Proxy m -> a -> Outside inc r m DrawDot }

instance (Draw inc r m a) => Sat (DrawDict inc r m a) where
	dict = DrawDict { drawDict = draw }
		
-- * Graphviz bindings

drawPDF :: Draw inc r m a => Proxy inc -> Proxy r -> Proxy m -> a -> Outside inc r m ()
drawPDF inc r m v = do
--	inL $ liftIO $ performGC >> performMajorGC >> threadDelay 2000000
	dir <- inL $ liftIO $ getTemporaryDirectory
	filename <- inL $ liftIO $ liftM toString $ nextUUIDSafe
	let pdfFile = dir </> addExtension filename "pdf"
	drawToPDF inc r m v pdfFile
	inL $ liftIO $ modifyIORef tempPDFs (pdfFile:)
	inL $ liftIO $ putStrLn $ "drew " ++ filename ++ ".pdf"

mergePDFsInto :: (MonadRef r m,MonadIO m,Layer Outside inc r m) => FilePath -> Outside inc r m ()
mergePDFsInto pdfFile = inL $ liftIO $ do
	pdfs <- atomicModifyIORef' tempPDFs (\pdfs -> ([],pdfs))
	system $ "pdftk " ++ unwords (reverse pdfs) ++ " cat output " ++ pdfFile
	return ()

------

drawToDot :: Draw inc r m a => Proxy inc -> Proxy r -> Proxy m -> a -> FilePath -> Outside inc r m ()
drawToDot inc r m v dotFile = do
	graph <- drawGraph inc r m v
	let txt = printDotGraph graph
	inL $ liftIO $ T.writeFile dotFile txt
	return ()
	
drawToPDF :: Draw inc r m a => Proxy inc -> Proxy r -> Proxy m -> a -> FilePath -> Outside inc r m ()
drawToPDF inc r m v pdfFile = do
	graph <- drawGraph inc r m v
	inL $ liftIO $ runGraphviz graph Pdf pdfFile
	return ()

drawGraph :: Draw inc r m a => Proxy inc -> Proxy r -> Proxy m -> a -> Outside inc r m (DotGraph String)
drawGraph inc r m x = do
	inL resetDrawnNodes
--	inL $ liftIO printAllRefs
	dot <- liftM (drawGraphStatements . snd) (draw inc r m x)
	inL resetDrawnNodes
	return dot

drawGraphStatements :: [DotStatement String] -> DotGraph String
drawGraphStatements dot = DotGraph {strictGraph = False, directedGraph = True, graphID = Just (Str $ T.pack "AdaptonGraph"), graphStatements = Seq.fromList $ attributes++dot }
	where attributes = [GA $ GraphAttrs {attrs = [Ordering OutEdges]}]

constructorNode :: String -> String -> DotNode String
constructorNode nodeID nodeLabel = DotNode {nodeID = nodeID, nodeAttributes = [Shape PlainText,Label (StrLabel $ T.pack nodeLabel)]}

constructorEdge :: String -> String -> DotEdge String
constructorEdge fromNode toNode = DotEdge {fromNode = fromNode, toNode = toNode, edgeAttributes = [Style [SItem Dashed []],ArrowHead (AType [(ArrMod {arrowFill = FilledArrow, arrowSide = BothSides},Tee)])]}

dependentEdge :: Bool -> String -> String -> DotEdge String
dependentEdge isDirty fromNode toNode = DotEdge {fromNode = fromNode, toNode = toNode, edgeAttributes = [Color [WC {wColor = X11Color color, weighting = Nothing}],ArrowHead (AType [(ArrMod {arrowFill = FilledArrow, arrowSide = LeftSide},Normal)])]}
	where color = if isDirty then Red else Black

dependencyEdge :: Bool -> String -> String -> DotEdge String
dependencyEdge isDirty fromNode toNode = DotEdge {fromNode = fromNode, toNode = toNode, edgeAttributes = [Style [SItem Tapered []],Color [WC {wColor = X11Color color, weighting = Nothing}],ArrowHead (AType [(ArrMod {arrowFill = FilledArrow, arrowSide = LeftSide},Normal)])]}
	where color = if isDirty then Red else Black

addEdgeLabel :: String -> DotEdge n -> DotEdge n
addEdgeLabel lbl edge = edge { edgeAttributes = Label (StrLabel $ T.pack lbl) : edgeAttributes edge }

sameRank :: [String] -> DotSubGraph String
sameRank nodeIDs = DotSG {isCluster = False, subGraphID = Nothing, subGraphStmts = Seq.fromList $ GA (GraphAttrs {attrs = [Rank SameRank]}) : nodes }
	where nodes = map (\nID -> DN (DotNode {nodeID = nID, nodeAttributes = []})) nodeIDs

mergeDrawDot :: Monad m => DrawDot -> DrawDot -> m DrawDot
mergeDrawDot (xs1,dot1) (xs2,dot2) = return (xs1++xs2,dot1++dot2)

lNode :: Bool -> String -> DotNode String
lNode isUnevaluated thunkID = DotNode {nodeID = thunkID, nodeAttributes = [Color [WC {wColor = color, weighting = Nothing}],Shape Square,Label (StrLabel $ T.pack thunkID),Style [SItem Filled []],FillColor [WC {wColor = fillcolor, weighting = Nothing}]]}
	where fillcolor = if isUnevaluated then X11Color CadetBlue4 else X11Color White
	      color = if isUnevaluated then X11Color Black else X11Color CadetBlue4

deadNode :: String -> DotNode String
deadNode i = DotNode {nodeID = i, nodeAttributes = [Color [WC {wColor = color, weighting = Nothing}],Shape Terminator,Label (StrLabel $ T.pack i),Style [SItem Filled []],FillColor [WC {wColor = fillcolor, weighting = Nothing}]]}
	where fillcolor = X11Color Goldenrod4
	      color = X11Color Goldenrod4

mNode :: String -> DotNode String
mNode thunkID = DotNode {nodeID = thunkID, nodeAttributes = [Color [WC {wColor = color, weighting = Nothing}],Shape MSquare,Label (StrLabel $ T.pack thunkID),Style [SItem Filled []],FillColor [WC {wColor = fillcolor, weighting = Nothing}]]}
	where fillcolor = X11Color White
	      color = X11Color DarkGreen
	
iuNode :: String -> DotNode String
iuNode thunkID = DotNode {nodeID = thunkID, nodeAttributes = [Color [WC {wColor = color, weighting = Nothing}],Shape DiamondShape,Label (StrLabel $ T.pack thunkID),Style [SItem Filled []],FillColor [WC {wColor = fillcolor, weighting = Nothing}]]}
	where fillcolor = X11Color White
	      color = X11Color Goldenrod4

uNode :: Maybe Bool -> String -> DotNode String
uNode isDirtyUnevaluated thunkID = DotNode {nodeID = thunkID, nodeAttributes = [Color [WC {wColor = color, weighting = Nothing}],Shape Circle,Label (StrLabel $ T.pack thunkID),Style [SItem Filled []],FillColor [WC {wColor = fillcolor, weighting = Nothing}]]}
	where fillcolor = X11Color $ case isDirtyUnevaluated of { Nothing -> CadetBlue4; Just True -> Red ; Just False -> White }
	      color = X11Color $ case isDirtyUnevaluated of { Nothing -> Black; Just True -> Black ; Just False -> CadetBlue4 }

nextUUIDSafe :: IO UUID
nextUUIDSafe = do
	mb <- nextUUID
	case mb of
		Nothing -> threadDelay 100 >> nextUUIDSafe
		Just uuid -> return uuid

-- special instance that simplifies the visualization of multiple values in the same graph 
instance (Draw inc r m a,Draw inc r m b) => Draw inc r m (Merge a b) where
	draw inc r m (Merge a b) = do
		draw1 <- draw inc r m a
		draw2 <- draw inc r m b
		(ids,dot) <- mergeDrawDot draw1 draw2
		return (ids,SG (sameRank ids) : dot)

instance (MonadIO m,Incremental inc r m) => Draw inc r m String where
	draw inc r m str = do
		parentID <- liftM toString $ inL $ liftIO $ nextUUIDSafe
		return ([parentID],[DN $ constructorNode parentID str])

instance (MonadIO m,Incremental inc r m,MData (DrawDict inc r m) (Outside inc r m) a) => Draw inc r m [a] where
	draw inc r m xs = do
		parentID <- liftM toString $ inL $ liftIO $ nextUUIDSafe
		let parentLabel = "[]"
		(childrenIDs,childrenDot) <- liftM mergeLists $ mapM (drawDict dict inc r m) xs
		let parentNode = DN $ constructorNode parentID parentLabel
		let childrenEdges = map (DE . constructorEdge parentID) childrenIDs
		let childrenRank = sameRank childrenIDs
		return ([parentID],parentNode : childrenEdges ++ SG childrenRank : childrenDot)

instance (MonadIO m,Incremental inc r m,MData (DrawDict inc r m) (Outside inc r m) a) => Draw inc r m (Set a) where
	draw inc r m (Set.toList -> xs) = do
		parentID <- liftM toString $ inL $ liftIO $ nextUUIDSafe
		let parentLabel = "Set"
		(childrenIDs,childrenDot) <- liftM mergeLists $ mapM (drawDict dict inc r m) xs
		let parentNode = DN $ constructorNode parentID parentLabel
		let childrenEdges = map (DE . constructorEdge parentID) childrenIDs
		let childrenRank = sameRank childrenIDs
		return ([parentID],parentNode : childrenEdges ++ SG childrenRank : childrenDot)

instance (MonadIO m,Incremental inc r m,MData (DrawDict inc r m) (Outside inc r m) b,Show a) => Draw inc r m (Map a b) where
	draw inc r m (Map.toList -> xs) = do
		let (keys,values) = unzip xs
		parentID <- liftM toString $ inL $ liftIO $ nextUUIDSafe
		let parentLabel = "Map"
		(childrenIDs,childrenDot) <- liftM mergeLists $ mapM (drawDict dict inc r m) values
		let parentNode = DN $ constructorNode parentID parentLabel
		childrenEdges <- mapM (\(k,childID) -> return $ DE $ addEdgeLabel (show k) $ constructorEdge parentID childID) $ zip keys childrenIDs
		let childrenRank = sameRank childrenIDs
		return ([parentID],parentNode : childrenEdges ++ SG childrenRank : childrenDot)

mergeLists l = let (xxs,yys) = unzip l in (concat xxs,concat yys)

drawProxy :: Proxy inc -> Proxy r -> Proxy m -> Proxy (DrawDict inc r m)
drawProxy inc r m = Proxy

-- default instance for arbitrary types
instance (Incremental inc r m,WeakRef r,MonadRef r m,MonadIO m,MData (DrawDict inc r m) (Outside inc r m) a) => Draw inc r m a where
	draw inc r m v = do
		parentID <- liftM toString $ inL $ liftIO $ nextUUIDSafe
		parentLabel <- liftM showConstr $ toConstr (drawProxy inc r m) v
		(childrenIDs,childrenDot) <- gmapQr (drawProxy inc r m) mergeDrawDot ([],[]) (drawDict dict inc r m) v
		let parentNode = DN $ constructorNode parentID parentLabel
		let childrenEdges = map (DE . constructorEdge parentID) childrenIDs
		let childrenRank = sameRank childrenIDs
		return ([parentID],parentNode : childrenEdges ++ SG childrenRank : childrenDot)

{-# LANGUAGE ViewPatterns, ScopedTypeVariables, DeriveDataTypeable, TemplateHaskell, OverlappingInstances, UndecidableInstances, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Control.Monad.Incremental.Draw where

import Control.Monad.Incremental
import Control.Monad.Incremental.Internal.Adapton.Algorithm
import Control.Monad.Incremental.Internal.Adapton.Layers
import Control.Monad.Incremental.Internal.Adapton.Types hiding (MData)
import Control.Monad.Ref
 as WeakSet
import Control.Monad.Trans

import Control.DeepSeq as Seq

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
import qualified Data.Text as Strict
import qualified Data.Text.IO as Strict
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Data.GraphViz.Types
import qualified Data.GraphViz.Types.Canonical as Can
import Data.GraphViz.Types.Generalised
import qualified Data.GraphViz.Types.Generalised as Gen
import Data.GraphViz.Attributes.HTML as HTML hiding (Attribute,Color)
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
import Data.Global.TH as TH

data Merge a b = Merge a b deriving (Typeable,Eq,Show,Ord) -- a special type that allows passing pairs of arguments to @drawToDot@

$( derive makeMData ''Merge )

$( derive makeDeepTypeable ''Merge )

--instance (DeepTypeable a,DeepTypeable b) => DeepTypeable (Merge a b) where
--	typeTree (_::Proxy (Merge a b)) = MkTypeTree (mkName "Control.Monad.Incremental.Draw.Merge") [MkConTree (mkName "Control.Monad.Incremental.Draw.Merge") [typeTree (Proxy :: Proxy a),typeTree (Proxy :: Proxy b)]]

-- hack to automatize drawing sequences

declareMVar "tempGraphs"  [t| [(FilePath,Bool)] |] [e| [] |]
declareMVar "drawnDependents"  [t| Map ThreadId [String] |] [e| Map.empty |]
declareMVar "drawnDependencies"  [t| Map ThreadId [String] |] [e| Map.empty |]

--checkDrawnDependencies :: (Layer l inc, m) => String -> l inc [a] -> l inc [a]
--checkDrawnDependencies node m = do
--	threadid <- liftIO myThreadId
--	nodesmap <- liftIO $ takeMVar drawnDependencies
----	let nodes = maybe [] id $ Map.lookup threadid nodesmap
--	if node `elem` nodesmap
--		then inL (liftIO $ putMVar drawnDependencies nodesmap) >> return []
--		else inL (liftIO $ putMVar drawnDependencies $ node:nodesmap) >> m

checkDrawnDependents :: String -> IO ([a],Map k v) -> IO ([a],Map k v)
checkDrawnDependents node m = do
	threadid <- myThreadId
	nodesmap <- takeMVar drawnDependents
	let nodes = maybe [] id $ Map.lookup threadid nodesmap
	if node `elem` nodes
		then (putMVar drawnDependents nodesmap) >> (return ([],Map.empty))
		else (putMVar drawnDependents $ Map.insertWith (++) threadid [node] nodesmap) >> m

checkDrawnDependentsLayer :: Layer l inc => String -> l inc ([a],Map k v) -> l inc ([a],Map k v)
checkDrawnDependentsLayer node m = do
	threadid <- unsafeIOToInc $ myThreadId
	nodesmap <- unsafeIOToInc $ takeMVar drawnDependents
	let nodes = maybe [] id $ Map.lookup threadid nodesmap
	if node `elem` nodes
		then (unsafeIOToInc $ putMVar drawnDependents nodesmap) >> (return ([],Map.empty))
		else (unsafeIOToInc $ putMVar drawnDependents $ Map.insertWith (++) threadid [node] nodesmap) >> m

resetDrawnNodes :: IO ()
resetDrawnNodes = do
	threadid <- liftIO myThreadId
	let delete = return . Map.delete threadid
--	let delete = \_ -> return []
	liftIO $ modifyMVar_ drawnDependencies delete >> modifyMVar_ drawnDependents delete

-- * Graphviz Drawing classes

-- a list of top-level ids and (elements of) a graph, and a table mapping node identifiers to read values
type DrawDot = ([String],[Gen.DotStatement String],Map String String)

class (Layer Outside inc) => Draw inc a where
	draw :: Proxy inc -> a -> Outside inc DrawDot

data DrawDict inc a = DrawDict { drawDict :: Proxy inc -> a -> Outside inc DrawDot }

instance (Draw inc a) => Sat (DrawDict inc a) where
	dict = DrawDict { drawDict = draw }
		
-- * Graphviz bindings

drawPDF :: Draw inc a => String -> Proxy inc -> a -> Outside inc ()
drawPDF label inc v = do
--	liftIO $ performGC >> threadDelay 2000000
	dir <- unsafeIOToInc getTemporaryDirectory -- $ return "/Users/hpacheco/Desktop/tmp/"
	filename <- unsafeIOToInc $ liftM toString $ nextUUIDSafe
	let pdfFile = dir </> addExtension filename "pdf"
	drawToPDF label inc v pdfFile
	unsafeIOToInc $ modifyMVar_ tempGraphs (return . ((pdfFile,True):))
--	liftIO $ putStrLn $ "drew " ++ filename ++ ".pdf"
	
drawDot :: Draw inc a => String -> Proxy inc -> a -> Outside inc ()
drawDot label inc v = do
--	liftIO $ performGC >> threadDelay 2000000
	dir <- unsafeIOToInc getTemporaryDirectory -- $ return "/Users/hpacheco/Desktop/tmp/"
	filename <- unsafeIOToInc $ liftM toString $ nextUUIDSafe
	let dotFile = dir </> addExtension filename "dot"
	drawToDot label inc v dotFile
	unsafeIOToInc $ modifyMVar_ tempGraphs (return . ((dotFile,False):))
--	liftIO $ putStrLn $ "drew " ++ filename ++ ".dot"

mergeGraphsInto :: (Layer Outside inc) => FilePath -> Outside inc ()
mergeGraphsInto = unsafeIOToInc . mergeGraphsInto'
	
dotToPDF :: FilePath -> IO FilePath
dotToPDF dot = do
	let pdf = replaceExtension dot "pdf"
	system $ "dot -Tpdf " ++ dot ++ " -o " ++ pdf
	return pdf
	
mergeGraphsInto' :: FilePath -> IO ()
mergeGraphsInto' pdfFile = do
	graphs <- modifyMVar tempGraphs (\pdfs -> return ([],pdfs))
	let convert (file,typ) = case typ of
		False -> dotToPDF file
		True -> return file
	pdfs <- mapM convert graphs
	system $ "pdftk " ++ unwords (reverse pdfs) ++ " cat output " ++ pdfFile
	return ()

------

drawToDot :: Draw inc a => String -> Proxy inc -> a -> FilePath -> Outside inc ()
drawToDot label inc v dotFile = do
	graph <- drawGraph label inc v
	let txt = Seq.force $ printDotGraph graph
	unsafeIOToInc $ T.writeFile dotFile txt
	return ()
	
drawToPDF :: Draw inc a => String -> Proxy inc -> a -> FilePath -> Outside inc ()
drawToPDF label inc v pdfFile = do
	graph <- drawGraph label inc v
	unsafeIOToInc $ runGraphviz graph Pdf pdfFile
	return ()

drawGraph :: Draw inc a => String -> Proxy inc -> a -> Outside inc (DotGraph String)
drawGraph label inc x = do
	unsafeIOToInc resetDrawnNodes
--	liftIO printAllRefs
	let labelNode = Gen.DN $ DotNode {nodeID = label, nodeAttributes = [Shape PlainText,Label (StrLabel $ T.pack label)]}
	dot <- liftM (\(x,y,z) -> drawGraphStatements $ labelNode : y ++ drawTable z) (draw inc x)
	unsafeIOToInc resetDrawnNodes
	return dot

drawTable :: Map String String -> [DotStatement String]
drawTable tbl = [DN (DotNode {nodeID = "memos", nodeAttributes = [Shape PlainText,Label (HtmlLabel (Table (HTable {tableFontAttrs = Nothing, tableAttrs = [Border 1,CellBorder 1,CellSpacing 0], tableRows = rows })))]})]
	where
	rows = Map.elems $ Map.mapWithKey drawCell tbl
	drawCell k v = HTML.Cells [LabelCell [] (Text [HTML.Str $ T.pack k]),HTML.LabelCell [] (Text [HTML.Str $ T.pack v])]
	
	


	


drawGraphStatements :: [Gen.DotStatement String] -> DotGraph String
drawGraphStatements dot = DotGraph {strictGraph = False, directedGraph = True, graphID = Just (Gen.Str $ T.pack "AdaptonGraph"), graphStatements = Seq.fromList $ attributes++dot }
	where attributes = [Gen.GA $ GraphAttrs {attrs = [Ordering OutEdges]}]

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
sameRank nodeIDs = DotSG {isCluster = False, subGraphID = Nothing, subGraphStmts = Seq.fromList $ Gen.GA (GraphAttrs {attrs = [Rank SameRank]}) : nodes }
	where nodes = map (\nID -> Gen.DN (DotNode {nodeID = nID, nodeAttributes = []})) nodeIDs

mergeDrawDot :: Monad m => DrawDot -> DrawDot -> m DrawDot
mergeDrawDot (xs1,dot1,tbl1) (xs2,dot2,tbl2) = return (xs1++xs2,dot1++dot2,Map.union tbl1 tbl2)

lNode :: Bool -> String -> DotNode String
lNode isUnevaluated thunkID = DotNode {nodeID = thunkID, nodeAttributes = [Color [WC {wColor = color, weighting = Nothing}],Shape Square,Label (StrLabel $ T.pack thunkID),Style [SItem Filled []],FillColor [WC {wColor = fillcolor, weighting = Nothing}]]}
	where fillcolor = if isUnevaluated then X11Color CadetBlue4 else X11Color White
	      color = if isUnevaluated then X11Color Black else X11Color CadetBlue4

deadNode :: String -> DotNode String
deadNode i = DotNode {nodeID = i, nodeAttributes = [Color [WC {wColor = color, weighting = Nothing}],Shape Terminator,Label (StrLabel $ T.pack i),Style [SItem Filled []],FillColor [WC {wColor = fillcolor, weighting = Nothing}]]}
	where fillcolor = X11Color Goldenrod4
	      color = X11Color Goldenrod4

mNode :: String -> String -> DotNode String
mNode thunkID label = DotNode {nodeID = thunkID, nodeAttributes = [Color [WC {wColor = color, weighting = Nothing}],Shape MSquare,Label (StrLabel $ T.pack $ thunkID ++ label),Style [SItem Filled []],FillColor [WC {wColor = fillcolor, weighting = Nothing}]]}
	where fillcolor = X11Color White
	      color = X11Color DarkGreen
	
iuNode :: String -> String -> DotNode String
iuNode thunkID label = DotNode {nodeID = thunkID, nodeAttributes = [Color [WC {wColor = color, weighting = Nothing}],Shape DiamondShape,Label (StrLabel $ T.pack $ thunkID ++ label),Style [SItem Filled []],FillColor [WC {wColor = fillcolor, weighting = Nothing}]]}
	where fillcolor = X11Color White
	      color = X11Color Goldenrod4

uNode :: Maybe Bool -> String -> String -> DotNode String
uNode isDirtyUnevaluated thunkID label = DotNode {nodeID = thunkID, nodeAttributes = [Color [WC {wColor = color, weighting = Nothing}],Shape Circle,Label (StrLabel $ T.pack $ thunkID ++ label),Style [SItem Filled []],FillColor [WC {wColor = fillcolor, weighting = Nothing}]]}
	where fillcolor = X11Color $ case isDirtyUnevaluated of { Nothing -> CadetBlue4; Just True -> Red ; Just False -> White }
	      color = X11Color $ case isDirtyUnevaluated of { Nothing -> Black; Just True -> Black ; Just False -> CadetBlue4 }

addAttribute :: Attribute -> DotNode String -> DotNode String
addAttribute att n = n { nodeAttributes = att : nodeAttributes n }

nextUUIDSafe :: IO UUID
nextUUIDSafe = do
	mb <- nextUUID
	case mb of
		Nothing -> threadDelay 100 >> nextUUIDSafe
		Just uuid -> return uuid

-- special instance that simplifies the visualization of multiple values in the same graph 
instance (Draw inc a,Draw inc b) => Draw inc (Merge a b) where
	draw inc (Merge a b) = do
		draw1 <- draw inc a
		draw2 <- draw inc b
		(ids,dot,table) <- mergeDrawDot draw1 draw2
		return (ids,Gen.SG (sameRank ids) : dot,table)

instance (Incremental inc) => Draw inc String where
	draw inc str = do
		parentID <- liftM toString $ unsafeIOToInc $ nextUUIDSafe
		return ([parentID],[Gen.DN $ constructorNode parentID str],Map.empty)

instance (Incremental inc,MData (DrawDict inc) (Outside inc) a) => Draw inc [a] where
	draw inc xs = do
		parentID <- liftM toString $ unsafeIOToInc $ nextUUIDSafe
		let parentLabel = "[]"
		(childrenIDs,childrenDot,childrenTable) <- liftM mergeLists $ mapM (drawDict dict inc) xs
		let parentNode = Gen.DN $ constructorNode parentID parentLabel
		let childrenEdges = map (Gen.DE . constructorEdge parentID) childrenIDs
		let childrenRank = sameRank childrenIDs
		return ([parentID],parentNode : childrenEdges ++ Gen.SG childrenRank : childrenDot,childrenTable)

instance (Incremental inc,MData (DrawDict inc) (Outside inc) a) => Draw inc (Set a) where
	draw inc (Set.toList -> xs) = do
		parentID <- liftM toString $ unsafeIOToInc $ nextUUIDSafe
		let parentLabel = "Set"
		(childrenIDs,childrenDot,childrenTable) <- liftM mergeLists $ mapM (drawDict dict inc) xs
		let parentNode = Gen.DN $ constructorNode parentID parentLabel
		let childrenEdges = map (Gen.DE . constructorEdge parentID) childrenIDs
		let childrenRank = sameRank childrenIDs
		return ([parentID],parentNode : childrenEdges ++ Gen.SG childrenRank : childrenDot,childrenTable)

instance (Incremental inc,MData (DrawDict inc) (Outside inc) b,Show a) => Draw inc (Map a b) where
	draw inc (Map.toList -> xs) = do
		let (keys,values) = unzip xs
		parentID <- liftM toString $ unsafeIOToInc $ nextUUIDSafe
		let parentLabel = "Map"
		(childrenIDs,childrenDot,childrenTable) <- liftM mergeLists $ mapM (drawDict dict inc) values
		let parentNode = Gen.DN $ constructorNode parentID parentLabel
		childrenEdges <- mapM (\(k,childID) -> return $ Gen.DE $ addEdgeLabel (show k) $ constructorEdge parentID childID) $ zip keys childrenIDs
		let childrenRank = sameRank childrenIDs
		return ([parentID],parentNode : childrenEdges ++ Gen.SG childrenRank : childrenDot,childrenTable)

mergeLists l = let (xxs,yys,zzs) = unzip3 l in (concat xxs,concat yys,foldr Map.union Map.empty zzs)

drawProxy :: Proxy inc -> Proxy (DrawDict inc)
drawProxy inc = Proxy

-- default instance for arbitrary types
instance (Incremental inc,MData (DrawDict inc) (Outside inc) a) => Draw inc a where
	draw inc v = do
		parentID <- liftM toString $ unsafeIOToInc $ nextUUIDSafe
		parentLabel <- liftM showConstr $ toConstr (drawProxy inc) v
		(childrenIDs,childrenDot,childrenTable) <- gmapQr (drawProxy inc) mergeDrawDot ([],[],Map.empty) (drawDict dict inc) v
		let parentNode = Gen.DN $ constructorNode parentID parentLabel
		let childrenEdges = map (Gen.DE . constructorEdge parentID) childrenIDs
		let childrenRank = sameRank childrenIDs
		return ([parentID],parentNode : childrenEdges ++ Gen.SG childrenRank : childrenDot,childrenTable)

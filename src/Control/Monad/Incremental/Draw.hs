{-# LANGUAGE ViewPatterns, ScopedTypeVariables, DeriveDataTypeable, TemplateHaskell, OverlappingInstances, UndecidableInstances, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Control.Monad.Incremental.Draw where

import Control.Monad.Incremental
import Control.Monad.Incremental.Adapton.Algorithm
import Control.Monad.Incremental.Adapton.Layers
import Control.Monad.Incremental.Adapton.Types hiding (MData)
import Control.Monad.Ref
import System.Mem.WeakSet as WeakSet
import Control.Monad.Trans
import System.Mem.WeakKey
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

--checkDrawnDependencies :: (Layer l inc r m,MonadIO m) => String -> l inc r m [a] -> l inc r m [a]
--checkDrawnDependencies node m = do
--	threadid <- inL $ liftIO myThreadId
--	nodesmap <- inL $ liftIO $ takeMVar drawnDependencies
----	let nodes = maybe [] id $ Map.lookup threadid nodesmap
--	if node `elem` nodesmap
--		then inL (liftIO $ putMVar drawnDependencies nodesmap) >> return []
--		else inL (liftIO $ putMVar drawnDependencies $ node:nodesmap) >> m

checkDrawnDependents :: (Layer l inc r m,MonadIO m) => String -> l inc r m ([a],Map k v) -> l inc r m ([a],Map k v)
checkDrawnDependents node m = do
	threadid <- inL $ liftIO myThreadId
	nodesmap <- inL $ liftIO $ takeMVar drawnDependents
	let nodes = maybe [] id $ Map.lookup threadid nodesmap
	if node `elem` nodes
		then inL (liftIO $ putMVar drawnDependents nodesmap) >> (return ([],Map.empty))
		else inL (liftIO $ putMVar drawnDependents $ Map.insertWith (++) threadid [node] nodesmap) >> m

resetDrawnNodes :: MonadIO m => m ()
resetDrawnNodes = do
	threadid <- liftIO myThreadId
	let delete = return . Map.delete threadid
--	let delete = \_ -> return []
	liftIO $ modifyMVar_ drawnDependencies delete >> modifyMVar_ drawnDependents delete

-- * Graphviz Drawing classes

-- a list of top-level ids and (elements of) a graph, and a table mapping node identifiers to read values
type DrawDot = ([String],[Gen.DotStatement String],Map String String)

class (MonadRef r m,MonadIO m,Layer Outside inc r m) => Draw inc r m a where
	draw :: Proxy inc -> Proxy r -> Proxy m -> a -> Outside inc r m DrawDot

data DrawDict inc r m a = DrawDict { drawDict :: Proxy inc -> Proxy r -> Proxy m -> a -> Outside inc r m DrawDot }

instance (Draw inc r m a) => Sat (DrawDict inc r m a) where
	dict = DrawDict { drawDict = draw }
		
-- * Graphviz bindings

drawPDF :: Draw inc r m a => String -> Proxy inc -> Proxy r -> Proxy m -> a -> Outside inc r m ()
drawPDF label inc r m v = do
--	inL $ liftIO $ performGC >> threadDelay 2000000
	dir <- inL $ liftIO getTemporaryDirectory -- $ return "/Users/hpacheco/Desktop/tmp/"
	filename <- inL $ liftIO $ liftM toString $ nextUUIDSafe
	let pdfFile = dir </> addExtension filename "pdf"
	drawToPDF label inc r m v pdfFile
	inL $ liftIO $ modifyMVar_ tempGraphs (return . ((pdfFile,True):))
--	inL $ liftIO $ putStrLn $ "drew " ++ filename ++ ".pdf"
	
drawDot :: Draw inc r m a => String -> Proxy inc -> Proxy r -> Proxy m -> a -> Outside inc r m ()
drawDot label inc r m v = do
--	inL $ liftIO $ performGC >> threadDelay 2000000
	dir <- inL $ liftIO getTemporaryDirectory -- $ return "/Users/hpacheco/Desktop/tmp/"
	filename <- inL $ liftIO $ liftM toString $ nextUUIDSafe
	let dotFile = dir </> addExtension filename "dot"
	drawToDot label inc r m v dotFile
	inL $ liftIO $ modifyMVar_ tempGraphs (return . ((dotFile,False):))
--	inL $ liftIO $ putStrLn $ "drew " ++ filename ++ ".dot"

mergeGraphsInto :: (MonadRef r m,MonadIO m,Layer Outside inc r m) => FilePath -> Outside inc r m ()
mergeGraphsInto = inL . liftIO . mergeGraphsInto'
	
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

drawToDot :: Draw inc r m a => String -> Proxy inc -> Proxy r -> Proxy m -> a -> FilePath -> Outside inc r m ()
drawToDot label inc r m v dotFile = do
	graph <- drawGraph label inc r m v
	let txt = Seq.force $ printDotGraph graph
	inL $ liftIO $ T.writeFile dotFile txt
	return ()
	
drawToPDF :: Draw inc r m a => String -> Proxy inc -> Proxy r -> Proxy m -> a -> FilePath -> Outside inc r m ()
drawToPDF label inc r m v pdfFile = do
	graph <- drawGraph label inc r m v
	inL $ liftIO $ runGraphviz graph Pdf pdfFile
	return ()

drawGraph :: Draw inc r m a => String -> Proxy inc -> Proxy r -> Proxy m -> a -> Outside inc r m (DotGraph String)
drawGraph label inc r m x = do
	inL resetDrawnNodes
--	inL $ liftIO printAllRefs
	let labelNode = Gen.DN $ DotNode {nodeID = label, nodeAttributes = [Shape PlainText,Label (StrLabel $ T.pack label)]}
	dot <- liftM (\(x,y,z) -> drawGraphStatements $ labelNode : y ++ drawTable z) (draw inc r m x)
	inL resetDrawnNodes
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
instance (Draw inc r m a,Draw inc r m b) => Draw inc r m (Merge a b) where
	draw inc r m (Merge a b) = do
		draw1 <- draw inc r m a
		draw2 <- draw inc r m b
		(ids,dot,table) <- mergeDrawDot draw1 draw2
		return (ids,Gen.SG (sameRank ids) : dot,table)

instance (MonadIO m,Incremental inc r m) => Draw inc r m String where
	draw inc r m str = do
		parentID <- liftM toString $ inL $ liftIO $ nextUUIDSafe
		return ([parentID],[Gen.DN $ constructorNode parentID str],Map.empty)

instance (MonadIO m,Incremental inc r m,MData (DrawDict inc r m) (Outside inc r m) a) => Draw inc r m [a] where
	draw inc r m xs = do
		parentID <- liftM toString $ inL $ liftIO $ nextUUIDSafe
		let parentLabel = "[]"
		(childrenIDs,childrenDot,childrenTable) <- liftM mergeLists $ mapM (drawDict dict inc r m) xs
		let parentNode = Gen.DN $ constructorNode parentID parentLabel
		let childrenEdges = map (Gen.DE . constructorEdge parentID) childrenIDs
		let childrenRank = sameRank childrenIDs
		return ([parentID],parentNode : childrenEdges ++ Gen.SG childrenRank : childrenDot,childrenTable)

instance (MonadIO m,Incremental inc r m,MData (DrawDict inc r m) (Outside inc r m) a) => Draw inc r m (Set a) where
	draw inc r m (Set.toList -> xs) = do
		parentID <- liftM toString $ inL $ liftIO $ nextUUIDSafe
		let parentLabel = "Set"
		(childrenIDs,childrenDot,childrenTable) <- liftM mergeLists $ mapM (drawDict dict inc r m) xs
		let parentNode = Gen.DN $ constructorNode parentID parentLabel
		let childrenEdges = map (Gen.DE . constructorEdge parentID) childrenIDs
		let childrenRank = sameRank childrenIDs
		return ([parentID],parentNode : childrenEdges ++ Gen.SG childrenRank : childrenDot,childrenTable)

instance (MonadIO m,Incremental inc r m,MData (DrawDict inc r m) (Outside inc r m) b,Show a) => Draw inc r m (Map a b) where
	draw inc r m (Map.toList -> xs) = do
		let (keys,values) = unzip xs
		parentID <- liftM toString $ inL $ liftIO $ nextUUIDSafe
		let parentLabel = "Map"
		(childrenIDs,childrenDot,childrenTable) <- liftM mergeLists $ mapM (drawDict dict inc r m) values
		let parentNode = Gen.DN $ constructorNode parentID parentLabel
		childrenEdges <- mapM (\(k,childID) -> return $ Gen.DE $ addEdgeLabel (show k) $ constructorEdge parentID childID) $ zip keys childrenIDs
		let childrenRank = sameRank childrenIDs
		return ([parentID],parentNode : childrenEdges ++ Gen.SG childrenRank : childrenDot,childrenTable)

mergeLists l = let (xxs,yys,zzs) = unzip3 l in (concat xxs,concat yys,foldr Map.union Map.empty zzs)

drawProxy :: Proxy inc -> Proxy r -> Proxy m -> Proxy (DrawDict inc r m)
drawProxy inc r m = Proxy

-- default instance for arbitrary types
instance (Incremental inc r m,WeakRef r,MonadRef r m,MonadIO m,MData (DrawDict inc r m) (Outside inc r m) a) => Draw inc r m a where
	draw inc r m v = do
		parentID <- liftM toString $ inL $ liftIO $ nextUUIDSafe
		parentLabel <- liftM showConstr $ toConstr (drawProxy inc r m) v
		(childrenIDs,childrenDot,childrenTable) <- gmapQr (drawProxy inc r m) mergeDrawDot ([],[],Map.empty) (drawDict dict inc r m) v
		let parentNode = Gen.DN $ constructorNode parentID parentLabel
		let childrenEdges = map (Gen.DE . constructorEdge parentID) childrenIDs
		let childrenRank = sameRank childrenIDs
		return ([parentID],parentNode : childrenEdges ++ Gen.SG childrenRank : childrenDot,childrenTable)

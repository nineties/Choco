------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Schedular where

import Choco
import Reg (Location, Reg(..) )
import Mach (Operation(..))
import Linearize (
    Inst(..), 
    InstDesc(..), 
    FunDec(..),
    endInst, consInst, consNop
    )
import Scheduling (
    opLatency,
    opIssueCycles,
    opInBasicBlock
    )
import Outputable
import Panic

import Control.Monad.State
import qualified Data.IntMap as I
import Data.List
import qualified Data.Map as M

{- instruction DAG -}
data Node = Node {
    node_inst       :: Inst,
    node_delay      :: Int,
    node_date       :: Int,
    node_length     :: Int,
    node_ancestors  :: Int,
    node_emitted_ancestors :: Int,
    node_index  :: Index
    }

emptyNode = Node {
    node_inst   = endInst,
    node_delay  = 0,
    node_date   = 0,
    node_length = -1,
    node_ancestors  = 0,
    node_emitted_ancestors = 0,
    node_index = -1
    }

instance Eq Node where
    n1 == n2 = node_index n1 == node_index n2

{- The scheular monad -}
type Index  = Int

data Env = Env {
    result_map  :: M.Map Location Index,
    uses_map    :: M.Map Location [Index],
    stores      :: [Index],
    loads       :: [Index],
    ready_queue :: [Index],
    node_graph  :: I.IntMap [(Index, Int)], -- list of son and delay
    node_table  :: I.IntMap Node,
    num_node    :: Int
    }

defaultEnv = Env {
    result_map  = M.empty,
    uses_map    = M.empty,
    stores      = [],
    loads       = [],
    ready_queue = [],
    node_graph  = I.empty,
    node_table  = I.empty,
    num_node    = 0
    }

type P a = StateT Env ChocoM a

initSchedular :: P ()
initSchedular = modify $ \env 
    -> env{ result_map = M.empty,
            uses_map   = M.empty,
            stores     = [],
            loads      = [],
            ready_queue = []
            }

getNode i = return . (I.! i) =<< gets node_table 

lookupResult :: Location -> P (Maybe (Index, Node))
lookupResult loc = do
  map <- gets result_map
  case M.lookup loc map of
    Just idx -> getNode idx >>= return . Just . ((,) idx)
    Nothing  -> return Nothing

lookupUses :: Location -> P [(Index, Node)]
lookupUses loc = do
    map   <- gets uses_map
    case M.lookup loc map of
         Just idxs -> do
             nodes <- mapM getNode idxs
             return (zip idxs nodes)
         Nothing -> return []

mkNode :: Inst -> Int -> Index -> Node
mkNode inst delay idx = emptyNode {
    node_inst   = inst,
    node_delay  = delay,
    node_index  = idx
    }

newNode :: Inst -> Int -> P (Index, Node)
newNode inst delay = do
    env@Env{ num_node = num, node_table = table } <- get
    let node = (mkNode inst delay num)

    put env{
        node_table = I.insert num node table,
        num_node   = num + 1
        }

    return (num, node)

updateNode :: Index -> (Node -> Node) -> P Node
updateNode idx f = do
    node <- getNode idx
    env@Env{ node_table = table } <- get
    put env{ node_table = I.insert idx (f node) table}
    return (f node)

putNode idx node = updateNode idx (const node)

addQueue :: Index -> P ()
addQueue idx = do
    env@Env{ ready_queue = queue } <- get
    put env{ ready_queue = idx:queue }

removeQueue :: Index -> P ()
removeQueue idx = do
    env@Env{ ready_queue = queue } <- get
    put env{ ready_queue = delete idx queue }

getSons :: Index -> P [(Index, Int)]
getSons idx = do
    graph <- gets node_graph
    return $ I.findWithDefault [] idx graph

{- Information of instructions -}
instLatency Inst{ idesc = Lop op } = opLatency op
instLatency _ = panic "instLatency"

instIssueCycles Inst{ idesc = Lop op } = opIssueCycles op
instIssueCycles _ = panic "instIssueCycles"

inBasicBlock Inst{ idesc = Lop op } = opInBasicBlock op
inBasicBlock _ = False

isLoad Inst{ idesc = Lop (Iload _) } = True
isLoad _ = False

isStore Inst{ idesc = Lop (Istore _) } = True
isStore _ = False

addEdge ancestor son get_delay = do
    delay <- return . get_delay =<< getNode ancestor

    env@Env{ node_graph = graph } <- get
    put env{ 
        node_graph =
            I.insert ancestor
            ((son, delay) : (I.findWithDefault [] ancestor graph))
            graph
            }

    updateNode son (\node ->
        node{ node_ancestors = node_ancestors node + 1 })
    return ()

addEdgeAfter son ancestor = addEdge ancestor son (const 0)

addInst inst =
    let 
    delay = instLatency inst

    in do
    (nodeid, node) <- newNode inst delay

    -- RAW dependencies
    forM (args inst) $ \r ->
        do result <- lookupResult (loc r)
           case result of
                Just (ancestor,_) -> addEdge ancestor nodeid node_delay
                Nothing -> return ()

    -- WAR dependencies
    forM (result inst) $ \r ->
        do uses <- lookupUses (loc r)
           mapM (addEdgeAfter nodeid) (map fst uses)

    -- WAW dependencies
    forM (result inst) $ \r ->
        do result <- lookupResult (loc r)
           case result of
                Just (ancestor,_) -> addEdgeAfter nodeid ancestor
                Nothing -> return ()

    if isLoad inst then do {
        env@Env{ loads = ls, stores = ss } <- get;

        mapM (addEdgeAfter nodeid) ss;

        put env{ loads = nodeid : ls };
    } else if isStore inst then do {
        env@Env{ loads = ls, stores = ss } <- get;

        mapM (addEdgeAfter nodeid) ss;
        mapM (addEdgeAfter nodeid) ls;

        put env{ loads = [], stores = [nodeid] };
    } else return ()

    -- remember the registers used and produced by this instruction
    forM (result inst) $ \r -> do
        env@Env{ result_map = map } <- get
        put env{ result_map = M.insert (loc r) nodeid map }

    forM (args inst) $ \r -> do
        env@Env{ uses_map = map } <- get
        let new_uses = case M.lookup (loc r) map of
                        Just nodes   -> nodeid : nodes
                        Nothing     -> [nodeid]
         in do
        put env{ uses_map = M.insert (loc r) new_uses map }

    node' <- getNode nodeid

    if node_ancestors node' == 0
       then addQueue nodeid -- all arguments are ready
       else return ()

{- entry point -}
fundecl :: FunDec -> ChocoM FunDec
fundecl f =
  do new_body <- evalStateT (schedule (fun_body f)) defaultEnv
     return FunDec {
       fun_name = fun_name f,
       fun_body = new_body
       }

schedule :: Inst -> P Inst
schedule inst@Inst{ idesc = Lend } = return inst
schedule inst | inBasicBlock inst
    = do initSchedular
         scheduleBlock inst
schedule inst
    = do next' <- schedule (next inst)
         return inst{ next = next' }

scheduleBlock inst 
    | inBasicBlock inst 
        = do addInst inst
             scheduleBlock (next inst)
    | otherwise =
        let
        critical_outputs = case (idesc inst) of
            Lop Icall_ind         -> [ head (args inst) ]
            Lop Itailcall_ind     -> [ head (args inst) ]
            Lop (Icall_imm _)     -> []
            Lop (Itailcall_imm _) -> []
            Lreturn               -> []
            _                     -> args inst
        in do
        queue <- gets ready_queue 
        mapM (longestPath critical_outputs) queue

        inst' <- schedule inst 
        modify $ \e -> e{ ready_queue = queue }
        reschedule 0 inst'

{- schedule a basic block, adding its instructions in front of the given
  instruction sequence -}
reschedule date cont = do
    queue <- mapM getNode =<< gets ready_queue

    if null queue then return cont else do

    -- lookup ready instruction
    case lookupReadyInst date queue of
         Nothing   -> reschedule (date + 1) cont
         Just node -> emit node

    where
    emit node = do
        -- remove node from queue
        removeQueue (node_index node)
        let issue_cycles = instIssueCycles (node_inst node)

        sons <- getSons (node_index node)

        mapM (\(sonid, delay) -> do
            let completion_date = date + issue_cycles + delay -1 
            son <- getNode sonid
            let new_date = if node_date son < completion_date
                          then completion_date
                          else node_date son

            son' <- putNode sonid son{ 
                node_date = new_date,
                node_emitted_ancestors = node_emitted_ancestors son + 1
                }

            if node_emitted_ancestors son' == node_ancestors son'
               then addQueue sonid
               else return()
               ) sons

        return . consInst
            (idesc.node_inst $ node) 
            (result.node_inst $ node)
            (args.node_inst $ node)
            =<< reschedule (date + issue_cycles) cont


lookupReadyInst date = extract emptyNode
    where
    extract best [] | best == emptyNode = Nothing
                    | otherwise         = Just best
    extract best (i:is)
        = if node_date i <= date && node_length i > node_length best
             then extract i is
             else extract best is

longestPath critical_outputs nodeid = do
    node <- getNode nodeid
    when (node_length node < 0) $ do
        sons <- getSons nodeid
        len <- case sons of
             [] -> do
                  if isCritical critical_outputs (result.node_inst $ node)
                     then return $ node_delay node
                     else return 0

             sons' -> 
                  foldM (\len (son, delay) -> do
                      pathlen <- longestPath critical_outputs son
                      return $ max len (pathlen + delay)
                      ) 0 sons'

        putNode nodeid node{ node_length = len }
        return ()

    return . node_length =<< getNode nodeid

isCritical critical_outputs results =
    or [loc r == loc o | r <- results, o <- critical_outputs]

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
    deriving (Show)

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
    puts        :: [Index],
    hs_writes   :: I.IntMap Index,
    hs_reads    :: I.IntMap [Index],
    node_graph  :: I.IntMap [(Index, Int)], -- list of son and delay
    node_table  :: I.IntMap Node,
    num_node    :: Int
    }

defaultEnv = Env {
    result_map  = M.empty,
    uses_map    = M.empty,
    stores      = [],
    loads       = [],
    puts        = [],
    hs_writes   = I.empty,
    hs_reads    = I.empty,
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
            puts       = [],
            hs_writes  = I.empty,
            hs_reads   = I.empty
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

modifyNode :: Index -> (Node -> Node) -> P Node
modifyNode idx f = do
    node <- getNode idx
    env@Env{ node_table = table } <- get
    put env{ node_table = I.insert idx (f node) table}
    return (f node)

putNode idx node = modifyNode idx (const node)

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

addEdge ancestor son get_delay = do
    delay <- return . get_delay =<< getNode ancestor

    env@Env{ node_graph = graph } <- get
    put env{ 
        node_graph =
            I.insert ancestor
            ((son, delay) : (I.findWithDefault [] ancestor graph))
            graph
            }

    modifyNode son (\node ->
        node{ node_ancestors = node_ancestors node + 1 })
    return ()

addEdgeAfter son ancestor = addEdge ancestor son (const 0)

addInst ready_queue inst = do
  let delay = instLatency inst
  (nodeid, node) <- newNode inst delay

  -- RAW depandencies
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

  case idesc inst of
    Lop (Iload _) -> do
      ss <- gets stores
      mapM (addEdgeAfter nodeid) ss
      modify $ \e -> e{ loads = nodeid : loads e }

    Lop (Istore _) -> do
      Env{ loads = ls, stores = ss } <- get
      mapM (addEdgeAfter nodeid) ss
      mapM (addEdgeAfter nodeid) ls
      modify $ \e -> e{ loads = [], stores = [nodeid] }

    Lop Iput -> do
      ps <- gets puts
      mapM (\p -> addEdge p nodeid (const 1)) ps
      modify $ \e -> e{ puts = nodeid : puts e }

    Lop (Ihsr n) -> do
      table <- gets hs_writes
      case I.lookup n table of
        Just node -> do
          addEdgeAfter nodeid node
          modify $ \e -> e{ hs_reads = I.adjust (nodeid :) n (hs_reads e) }
        Nothing -> 
          modify $ \e -> e{ hs_reads = I.insert n [nodeid] (hs_reads e) }

    Lop (Ihsw n) -> do
      table <- gets hs_writes
      case I.lookup n table of
        Just writ -> addEdgeAfter nodeid writ
        Nothing -> return ()
      table <- gets hs_reads
      case I.lookup n table of
        Just reads -> mapM_ (addEdgeAfter nodeid) reads
        Nothing -> return ()
      modify $ \e -> e{
        hs_writes = I.insert n nodeid (hs_writes e),
        hs_reads  = I.delete n (hs_reads e)
        }
    _ -> return ()

  -- remember the registers used and produced by this instruction
  forM (result inst) $ \r -> 
    modify $ \e -> e{ result_map = M.insert (loc r) nodeid (result_map e) }

  forM (args inst) $ \r -> do
    env@Env{ uses_map = map } <- get
    let new_uses = nodeid : M.findWithDefault [] (loc r) map
    put env{ uses_map = M.insert (loc r) new_uses map }

  node' <- getNode nodeid

  if node_ancestors node' == 0
    then return (nodeid : ready_queue)  -- all arguments are ready
    else return ready_queue

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
         scheduleBlock [] inst
schedule inst
    = do next' <- schedule (next inst)
         return inst{ next = next' }

scheduleBlock ready_queue inst 
    | inBasicBlock inst 
        = do ready_queue' <- addInst ready_queue inst
             scheduleBlock ready_queue' (next inst)
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
        mapM (longestPath critical_outputs) ready_queue

        inst' <- schedule inst 
        reschedule ready_queue 0 inst'

{- schedule a basic block, adding its instructions in front of the given
  instruction sequence -}
reschedule :: [Index] -> Int -> Inst -> P Inst
reschedule [] date cont = return cont
reschedule ready_queue date cont = do
  -- lookup ready instruction
  inst_queue <- mapM getNode ready_queue
  case lookupReadyInst date inst_queue of
    Nothing -> reschedule ready_queue (date + 1) cont
    Just node -> do
      -- remove node from queue
      let new_queue = delete (node_index node) ready_queue
      let issue_cycles = instIssueCycles (node_inst node)
      sons <- getSons (node_index node)
      new_queue2 <- foldM (\queue (sonid, delay) -> do
        let completion_date = date + issue_cycles + delay - 1
        son <- getNode sonid
        let new_date = if node_date son < completion_date 
                          then completion_date
                          else node_date son
        son' <- putNode sonid son{
          node_date = new_date,
          node_emitted_ancestors = node_emitted_ancestors son + 1
          }
        if node_emitted_ancestors son' == node_ancestors son'
          then return (sonid : queue)
          else return queue
        ) new_queue sons
      return . consInst
        (idesc.node_inst $ node)
        (result.node_inst $ node)
        (args.node_inst $ node)
          =<< reschedule new_queue2 (date + issue_cycles) cont

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

        modifyNode nodeid $ \n -> n{ node_length = len }
        return ()

    return . node_length =<< getNode nodeid

isCritical critical_outputs results =
    or [loc r == loc o | r <- results, o <- critical_outputs]

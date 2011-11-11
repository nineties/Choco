------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module MemAnal where

import Choco
import Const
import LamSyn
import Panic

import Control.Monad.State
import qualified Data.Map as M


type ID = Int

data MemStruct
  = Link ID
  | Word
  | Pointer
  | Array (Maybe Int) MemStruct
  | Tuple [MemStruct]

data Env = Env {
  vtable  :: M.Map Var (Maybe ID, ID),
  structs :: M.Map ID MemStruct,
  counter :: ID
  }

type P a = StateT Env ChocoM a

newID :: P ID
newID = do
  c <- gets counter
  modify $ \e -> e{ counter = c + 1 }
  return c

analyze :: Lambda -> P (Lambda, MemStruct)
analyze lam = case lam of
  Llet str (v, PcreateArray _ _ [size, init]) cont 
    -> case size of
        Llit (IntC n) -> do
          id <- newID
          (init', s1) <- analyze init
          modify $ \e -> e{
            vtable = M.insert v (Nothing, id) (vtable e),
            structs = M.insert id (Array (Just n) s1) (structs e)
            }
          (cont', s2) <- analyze cont
          return (Llet str (v, 

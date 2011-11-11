------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module RegM where

import Choco
import CmmSyn
import Reg

import Control.Monad.State
import qualified Data.Map as M

newRegId = do 
  id <- gets reg_stamp
  modify $ \e -> e{ reg_stamp = id + 1 }
  return id

addReg :: Reg -> ChocoM ()
addReg reg = do
    env@CEnv{ reg_list = regs, reg_info_table = tab } <- get
    put env{ reg_list = reg:regs, 
             reg_info_table = M.insert reg emptyRegInfo tab }

createRegv :: Bool -> ChocoM [Reg]
createRegv False = return []
createRegv True  = createReg >>= return . (: [])

createReg :: ChocoM Reg
createReg = do
    id <- newRegId
    let r = emptyReg {
            stamp = id
            }
    addReg r
    return r

copyReg :: Reg -> ChocoM Reg
copyReg r = do
    new <- createReg 
    i <- getRegInfo r
    modifyRegInfo new (const i)
    return new{ name = name r }


getsRegInfo :: Reg -> (RegInfo -> a) -> ChocoM a
getsRegInfo r f = getRegInfo r >>= return . f

getRegInfo :: Reg -> ChocoM RegInfo
getRegInfo r = gets reg_info_table >>= return . (M.! r)

modifyRegInfo :: Reg -> (RegInfo -> RegInfo) -> ChocoM ()
modifyRegInfo r f = do
  e@CEnv{ reg_info_table = tab } <- get
  let info  = tab M.! r
  put e{ reg_info_table = M.insert r (f info) tab }

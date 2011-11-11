------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Var (
  Name, -- abstract
  mkName, unName, 
  Var(..), mkVar, mkTmpVar, mkGlobalVar, copyVar, isGlobal, toSymbol,
  VarMap, mkIdent
  ) where

import Choco
import Id
import Outputable
import Types

import Data.Function
import qualified Data.Map as M
import Data.Maybe

{- name only -}
data Name = Name !String
    deriving (Eq, Ord)

mkName :: String -> Name
mkName name = Name name

unName :: Name -> String
unName (Name name) = name


instance Outputable Name where
    ppr (Name name) = text name

instance Show Name where
    show = show.ppr


{- variable -}
data Var = Var {
    var_name  :: String,
    var_id    :: Id,
    var_type  :: TyScheme,
    is_global :: Bool
    }

instance Eq Var where
  (==) = (==) `on` var_id

instance Ord Var where
  compare = compare `on` var_id

instance Outputable Var where
    ppr (Var name id ty False) = text name <> char '_' <> int id
    ppr (Var name id ty True) = char 'G' <> text name <> char '_' <> int id

instance Show Var where
    show = show.ppr

mkVar :: String -> Id -> TyScheme -> Bool -> Var
mkVar name id ty gbl = Var {
    var_name  = name,
    var_id    = id,
    var_type  = ty,
    is_global = gbl
    }

mkTmpVar :: String -> TyScheme -> ChocoM Var
mkTmpVar name ty
  = do id <- newUniq
       return $ mkVar name id ty False

mkGlobalVar :: String -> TyScheme -> ChocoM Var
mkGlobalVar name ty
  = do id <- newUniq
       return $ mkVar name id ty True

copyVar :: Var -> ChocoM Var
copyVar v = do id <- newUniq
               return $ mkVar (var_name v) id (var_type v) (is_global v)

isGlobal :: Var -> Bool
isGlobal v = is_global v

-- generate unique string
toSymbol :: Var -> String
toSymbol v  = var_name v ++ "_" ++ show (var_id v)

mkIdent :: String -> ChocoM String
mkIdent name = do
  id <- newUniq
  return (name ++ "!" ++ show id)

type VarMap = M.Map Name (TyScheme, Id, Bool)

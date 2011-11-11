------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Arch (
    archName,
    sizeAddr, sizeInt, sizeFloat,
    AddressingMode(..),
    hsramSize
    ) where

archName = "tsubaki"

sizeAddr, sizeInt, sizeFloat :: Int
sizeAddr    = 4
sizeInt     = 4
sizeFloat   = 4

data AddressingMode
    = Ibased String Int
    | Iindexed Int
    deriving (Eq, Ord, Show)

hsramSize :: Int
hsramSize = 128

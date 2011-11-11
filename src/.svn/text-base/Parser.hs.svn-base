{-# OPTIONS -fglasgow-exts -cpp #-}
{-# OPTIONS_GHC -w #-}
------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Parser 
    ( parse
    , parseExpr
    ) where
import Lexer
import SrcLoc
import Const
import McSyn
import Id

{- MinCaml parser -}
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
#else
import Array
#endif
#if __GLASGOW_HASKELL__ >= 503
import GHC.Exts
#else
import GlaExts
#endif

-- parser produced by Happy Version 1.16

newtype HappyAbsSyn  = HappyAbsSyn (() -> ())
happyIn5 :: ([ LMcStmt ]) -> (HappyAbsSyn )
happyIn5 x = unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> ([ LMcStmt ])
happyOut5 x = unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: ([ LMcStmt ]) -> (HappyAbsSyn )
happyIn6 x = unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> ([ LMcStmt ])
happyOut6 x = unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: (LMcStmt) -> (HappyAbsSyn )
happyIn7 x = unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> (LMcStmt)
happyOut7 x = unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: (Located Name) -> (HappyAbsSyn )
happyIn8 x = unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> (Located Name)
happyOut8 x = unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: (Located Name) -> (HappyAbsSyn )
happyIn9 x = unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> (Located Name)
happyOut9 x = unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: (Located Const) -> (HappyAbsSyn )
happyIn10 x = unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> (Located Const)
happyOut10 x = unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: (Located Const) -> (HappyAbsSyn )
happyIn11 x = unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> (Located Const)
happyOut11 x = unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: (LMcExpr) -> (HappyAbsSyn )
happyIn12 x = unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> (LMcExpr)
happyOut12 x = unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: (LMcExpr) -> (HappyAbsSyn )
happyIn13 x = unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> (LMcExpr)
happyOut13 x = unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: (Located [LMcExpr]) -> (HappyAbsSyn )
happyIn14 x = unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> (Located [LMcExpr])
happyOut14 x = unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: (LMcExpr) -> (HappyAbsSyn )
happyIn15 x = unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> (LMcExpr)
happyOut15 x = unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: (Located [ LMcExpr ]) -> (HappyAbsSyn )
happyIn16 x = unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> (Located [ LMcExpr ])
happyOut16 x = unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: (RecFlag) -> (HappyAbsSyn )
happyIn17 x = unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> (RecFlag)
happyOut17 x = unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: (Located [ (LMcPat, LMcExpr) ]) -> (HappyAbsSyn )
happyIn18 x = unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> (Located [ (LMcPat, LMcExpr) ])
happyOut18 x = unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: (Located (LMcPat, LMcExpr)) -> (HappyAbsSyn )
happyIn19 x = unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> (Located (LMcPat, LMcExpr))
happyOut19 x = unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: (LMcExpr) -> (HappyAbsSyn )
happyIn20 x = unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> (LMcExpr)
happyOut20 x = unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: (LMcPat) -> (HappyAbsSyn )
happyIn21 x = unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> (LMcPat)
happyOut21 x = unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: (LMcPat) -> (HappyAbsSyn )
happyIn22 x = unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> (LMcPat)
happyOut22 x = unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: (Located [LMcPat]) -> (HappyAbsSyn )
happyIn23 x = unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> (Located [LMcPat])
happyOut23 x = unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: (Located [(LMcPat, LMcExpr)]) -> (HappyAbsSyn )
happyIn24 x = unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> (Located [(LMcPat, LMcExpr)])
happyOut24 x = unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: (LMcExpr) -> (HappyAbsSyn )
happyIn25 x = unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> (LMcExpr)
happyOut25 x = unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyIn26 :: (()) -> (HappyAbsSyn )
happyIn26 x = unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> (())
happyOut26 x = unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyIn27 :: (Located String) -> (HappyAbsSyn )
happyIn27 x = unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> (Located String)
happyOut27 x = unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: (Located Name) -> (HappyAbsSyn )
happyIn28 x = unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> (Located Name)
happyOut28 x = unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyInTok :: Located Token -> (HappyAbsSyn )
happyInTok x = unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> Located Token
happyOutTok x = unsafeCoerce# x
{-# INLINE happyOutTok #-}

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x6d\x00\x2c\x00\xfe\xff\x00\x00\xfe\xff\xc3\x00\x62\x00\x97\x00\x00\x00\x00\x00\xfe\xff\xf8\x00\xad\x00\x90\x00\x37\x00\x00\x00\x00\x00\x00\x00\xbe\x00\x37\x00\x37\x00\x37\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x94\x00\xba\x00\x8b\x00\x00\x00\xa9\x00\xa3\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x00\x9d\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa4\x00\xa2\x00\x9f\x00\x8b\x00\x7f\x00\x6c\x00\x80\x00\x7b\x00\x37\x00\x37\x00\x37\x00\x37\x00\x37\x00\x37\x00\x37\x00\x37\x00\x37\x00\x37\x00\x37\x00\x37\x00\x37\x00\x37\x00\x37\x00\x00\x00\xfe\xff\xfe\xff\x8b\x00\x00\x00\xdc\x00\x00\x00\x00\x00\x6f\x00\x00\x00\xef\xff\x00\x00\x61\x00\x00\x00\xc1\x00\x7a\x00\x00\x00\x00\x00\x5f\x00\x7f\x02\x7f\x02\xf4\xff\xf4\xff\xf4\xff\x7f\x02\x7f\x02\x24\x01\x7f\x02\x00\x00\x24\x01\x0e\x01\x3a\x01\x24\x01\x37\x00\x55\x00\x57\x00\x0c\x00\x37\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x0c\x00\x37\x00\x8b\x00\x8b\x00\x00\x00\xaa\x00\x37\x00\x52\x00\x00\x00\x00\x00\x00\x00\xfc\xff\x7a\x00\x00\x00\x8b\x00\x8b\x00\x37\x00\x00\x00\xdc\x00\x37\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x29\x00\x48\x00\x37\x00\x26\x00\x30\x00\x00\x00\x00\x00\x8b\x00\x00\x00\x37\x00\x0e\x01\x00\x00\x37\x00\x0e\x01\x00\x00\x26\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xf8\x01\xf2\x00\x7c\x00\x00\x00\x70\x00\x44\x00\x98\x00\x00\x00\x00\x00\x00\x00\x4a\x00\x00\x00\x00\x00\x03\x00\x88\x02\x00\x00\x00\x00\x00\x00\x3d\x00\xf0\x01\xdb\x01\xd3\x01\x5f\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x32\x00\xbe\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xae\x02\x00\x00\x00\x00\x31\x00\x00\x00\x80\x02\x78\x02\x70\x02\x68\x02\xbe\x01\x60\x02\x58\x02\x50\x02\x48\x02\x40\x02\x38\x02\x30\x02\x28\x02\x20\x02\x18\x02\x00\x00\x2f\x00\x17\x00\xa8\x02\x00\x00\xdb\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x76\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb6\x01\x00\x00\x00\x00\x00\x00\x10\x02\x00\x00\x1e\x00\x00\x00\x00\x00\x00\x00\xa1\x01\xc3\x02\x97\x02\x00\x00\x00\x00\x99\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd7\x02\xd3\x02\x84\x01\x00\x00\x58\x00\x7c\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x02\x0a\x00\x00\x00\x00\x00\x00\x00\x41\x00\x00\x00\x67\x01\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\xfb\xff\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\xfb\xff\x00\x00\xfd\xff\xfb\xff\xc0\xff\xfa\xff\x00\x00\xc8\xff\xc7\xff\xfb\xff\xe1\xff\xda\xff\xde\xff\x00\x00\xc6\xff\xa7\xff\xa6\xff\xc0\xff\x00\x00\x00\x00\x00\x00\x00\x00\xab\xff\xaa\xff\xf5\xff\xa9\xff\xe6\xff\xe5\xff\x00\x00\xc0\xff\x00\x00\xbf\xff\x00\x00\x00\x00\xe7\xff\xa8\xff\xec\xff\xeb\xff\xe8\xff\xea\xff\xe9\xff\xef\xff\xab\xff\xaa\xff\xf3\xff\xf2\xff\xf1\xff\xf0\xff\x00\x00\x00\x00\x00\x00\x00\x00\xcc\xff\xc2\xff\xdd\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe0\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfc\xff\xfb\xff\xfb\xff\x00\x00\xf7\xff\xb6\xff\xe4\xff\xb4\xff\xf6\xff\xbe\xff\x00\x00\xb8\xff\xb7\xff\xb5\xff\x00\x00\x00\x00\xf9\xff\xf8\xff\xd6\xff\xd7\xff\xd8\xff\xd3\xff\xd4\xff\xd5\xff\xcf\xff\xd0\xff\xce\xff\xd1\xff\xdf\xff\xc9\xff\xd2\xff\xcd\xff\xca\xff\x00\x00\xc1\xff\x00\x00\xf6\xff\x00\x00\xc4\xff\xad\xff\xc5\xff\xf4\xff\x00\x00\x00\x00\x00\x00\x00\x00\xac\xff\x00\x00\x00\x00\x00\x00\xe3\xff\xe2\xff\xb6\xff\x00\x00\xee\xff\xed\xff\x00\x00\x00\x00\x00\x00\xbc\xff\x00\x00\x00\x00\xba\xff\xb9\xff\xbb\xff\xb1\xff\xb2\xff\xb3\xff\xc3\xff\x00\x00\x00\x00\x00\x00\xdb\xff\xbd\xff\xdc\xff\x00\x00\xb0\xff\x00\x00\xd9\xff\xc3\xff\x00\x00\xcb\xff\xae\xff\x00\x00\xaf\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x03\x00\x01\x00\x02\x00\x03\x00\x16\x00\x03\x00\x06\x00\x05\x00\x15\x00\x09\x00\x1c\x00\x0b\x00\x0a\x00\x0b\x00\x14\x00\x14\x00\x05\x00\x16\x00\x12\x00\x13\x00\x14\x00\x15\x00\x19\x00\x01\x00\x02\x00\x17\x00\x27\x00\x10\x00\x1c\x00\x14\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x01\x00\x02\x00\x03\x00\x01\x00\x02\x00\x06\x00\x15\x00\x03\x00\x09\x00\x05\x00\x0b\x00\x01\x00\x02\x00\x03\x00\x0a\x00\x16\x00\x06\x00\x0c\x00\x13\x00\x09\x00\x1b\x00\x0b\x00\x1a\x00\x03\x00\x19\x00\x05\x00\x06\x00\x17\x00\x0c\x00\x13\x00\x01\x00\x02\x00\x1d\x00\x22\x00\x23\x00\x0c\x00\x10\x00\x11\x00\x12\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x1d\x00\x22\x00\x23\x00\x03\x00\x14\x00\x05\x00\x06\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x01\x00\x02\x00\x03\x00\x14\x00\x0f\x00\x06\x00\x11\x00\x13\x00\x09\x00\x17\x00\x0b\x00\x01\x00\x02\x00\x03\x00\x01\x00\x02\x00\x06\x00\x15\x00\x13\x00\x09\x00\x16\x00\x0b\x00\x03\x00\x04\x00\x05\x00\x06\x00\x01\x00\x02\x00\x10\x00\x13\x00\x01\x00\x02\x00\x17\x00\x22\x00\x23\x00\x10\x00\x11\x00\x12\x00\x09\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x13\x00\x22\x00\x23\x00\x01\x00\x02\x00\x13\x00\x15\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x09\x00\x02\x00\x03\x00\x11\x00\x05\x00\x13\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x13\x00\x2a\x00\x2b\x00\x07\x00\x17\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x0a\x00\x22\x00\x16\x00\x17\x00\x0c\x00\x14\x00\x08\x00\x28\x00\x14\x00\x2a\x00\x2b\x00\x14\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x12\x00\x14\x00\x04\x00\x15\x00\x16\x00\x2d\x00\x04\x00\x16\x00\x2d\x00\xff\xff\x1c\x00\x04\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\xff\xff\x25\x00\x26\x00\x27\x00\x11\x00\x12\x00\x13\x00\xff\xff\x15\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1c\x00\xff\xff\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\xff\xff\x2a\x00\x2b\x00\x11\x00\xff\xff\x13\x00\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x05\x00\x1c\x00\x07\x00\x08\x00\x09\x00\x0a\x00\xff\xff\x22\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x28\x00\xff\xff\x2a\x00\x2b\x00\x16\x00\x17\x00\x12\x00\xff\xff\xff\xff\x15\x00\x16\x00\xff\xff\x18\x00\xff\xff\xff\xff\xff\xff\x1c\x00\xff\xff\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\xff\xff\x25\x00\x26\x00\x27\x00\x12\x00\xff\xff\xff\xff\x15\x00\x16\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1c\x00\xff\xff\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\xff\xff\x25\x00\x26\x00\x27\x00\x12\x00\xff\xff\xff\xff\x15\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1c\x00\xff\xff\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\xff\xff\x25\x00\x26\x00\x27\x00\x12\x00\xff\xff\xff\xff\x15\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1c\x00\xff\xff\xff\xff\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\xff\xff\x25\x00\x26\x00\x27\x00\x03\x00\x04\x00\x05\x00\xff\xff\x07\x00\x08\x00\x09\x00\x0a\x00\x03\x00\xff\xff\x05\x00\xff\xff\x07\x00\x08\x00\x09\x00\x0a\x00\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x03\x00\xff\xff\x05\x00\xff\xff\x07\x00\x08\x00\x09\x00\x0a\x00\x03\x00\xff\xff\x05\x00\xff\xff\x07\x00\x08\x00\x09\x00\x0a\x00\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x03\x00\xff\xff\x05\x00\xff\xff\x07\x00\x08\x00\x09\x00\x0a\x00\x03\x00\xff\xff\x05\x00\xff\xff\x07\x00\x08\x00\x09\x00\x0a\x00\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x03\x00\xff\xff\x05\x00\xff\xff\x07\x00\x08\x00\x09\x00\x0a\x00\x03\x00\xff\xff\x05\x00\xff\xff\x07\x00\x08\x00\x09\x00\x0a\x00\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x03\x00\xff\xff\x05\x00\xff\xff\x07\x00\x08\x00\x09\x00\x0a\x00\x03\x00\xff\xff\x05\x00\xff\xff\x07\x00\x08\x00\x09\x00\x0a\x00\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x03\x00\xff\xff\x05\x00\xff\xff\x07\x00\x08\x00\x09\x00\x0a\x00\x03\x00\xff\xff\x05\x00\xff\xff\x07\x00\x08\x00\x09\x00\x0a\x00\x03\x00\xff\xff\x05\x00\x16\x00\x17\x00\x08\x00\x09\x00\x0a\x00\x03\x00\xff\xff\x05\x00\x16\x00\x17\x00\x08\x00\x09\x00\x0a\x00\x03\x00\xff\xff\x05\x00\x16\x00\x17\x00\x08\x00\x09\x00\x0a\x00\x03\x00\xff\xff\x05\x00\x16\x00\x17\x00\x08\x00\x09\x00\x0a\x00\x03\x00\xff\xff\x05\x00\x16\x00\x17\x00\x08\x00\x09\x00\x0a\x00\x03\x00\xff\xff\x05\x00\x16\x00\x17\x00\x08\x00\x09\x00\x0a\x00\x03\x00\xff\xff\x05\x00\x16\x00\x17\x00\x08\x00\x09\x00\x0a\x00\x03\x00\xff\xff\x05\x00\x16\x00\x17\x00\x08\x00\x09\x00\x0a\x00\x03\x00\xff\xff\x05\x00\x16\x00\x17\x00\x08\x00\x09\x00\x0a\x00\x03\x00\xff\xff\x05\x00\x16\x00\x17\x00\x08\x00\x09\x00\x0a\x00\x03\x00\xff\xff\x05\x00\x16\x00\x17\x00\x08\x00\x09\x00\x0a\x00\x03\x00\xff\xff\x05\x00\x16\x00\x17\x00\x08\x00\x09\x00\x0a\x00\x03\x00\xff\xff\x05\x00\x16\x00\x17\x00\x08\x00\x09\x00\x0a\x00\x03\x00\xff\xff\x05\x00\x16\x00\x17\x00\x08\x00\x09\x00\x0a\x00\x03\x00\xff\xff\x05\x00\x16\x00\x17\x00\x08\x00\x09\x00\x0a\x00\x03\x00\xff\xff\x05\x00\x16\x00\x17\x00\x08\x00\x09\x00\x0a\x00\x03\x00\xff\xff\x05\x00\x16\x00\x17\x00\x08\x00\x09\x00\x0a\x00\x03\x00\xff\xff\x05\x00\x16\x00\x17\x00\x08\x00\x09\x00\x0a\x00\xff\xff\x15\x00\xff\xff\x16\x00\x17\x00\xff\xff\xff\xff\x03\x00\xff\xff\x05\x00\x06\x00\x16\x00\x17\x00\x21\x00\x22\x00\x23\x00\xff\xff\xff\xff\x26\x00\x27\x00\x10\x00\x11\x00\x12\x00\x13\x00\x03\x00\xff\xff\x05\x00\x06\x00\xff\xff\xff\xff\x03\x00\xff\xff\x05\x00\x06\x00\x0d\x00\x0e\x00\xff\xff\x10\x00\x11\x00\x12\x00\x0d\x00\x0e\x00\xff\xff\x10\x00\x11\x00\x12\x00\x03\x00\xff\xff\x05\x00\x06\x00\xff\xff\x03\x00\xff\xff\x05\x00\x06\x00\xff\xff\x0d\x00\x0e\x00\xff\xff\x10\x00\x11\x00\x12\x00\x0e\x00\xff\xff\x10\x00\x11\x00\x12\x00\x03\x00\xff\xff\x05\x00\x06\x00\x03\x00\xff\xff\x05\x00\x06\x00\x03\x00\xff\xff\x05\x00\x06\x00\xff\xff\x10\x00\x11\x00\x12\x00\xff\xff\x10\x00\x11\x00\x12\x00\x0f\x00\xff\xff\x11\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x06\x00\x11\x00\x12\x00\x1f\x00\x82\x00\x08\x00\x14\x00\x09\x00\x3c\x00\x15\x00\x83\x00\x16\x00\x36\x00\x37\x00\x9c\x00\x8c\x00\x74\x00\x82\x00\x24\x00\x17\x00\x25\x00\x26\x00\x07\x00\x58\x00\x04\x00\x0f\x00\x48\x00\x75\x00\x27\x00\x94\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x11\x00\x12\x00\x13\x00\x59\x00\x04\x00\x14\x00\x75\x00\x08\x00\x15\x00\x09\x00\x16\x00\x11\x00\x12\x00\x1f\x00\x6a\x00\x82\x00\x14\x00\x1f\x00\x17\x00\x15\x00\x96\x00\x16\x00\x99\x00\x7c\x00\x07\x00\x4e\x00\x4f\x00\x0f\x00\x34\x00\x17\x00\x48\x00\x04\x00\x94\x00\x18\x00\x19\x00\x4b\x00\x9b\x00\x53\x00\x54\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x77\x00\x18\x00\x19\x00\x7c\x00\x98\x00\x4e\x00\x4f\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x11\x00\x12\x00\x13\x00\x8d\x00\x87\x00\x14\x00\x84\x00\x79\x00\x15\x00\x6c\x00\x16\x00\x11\x00\x12\x00\x1f\x00\x4c\x00\x04\x00\x14\x00\x3c\x00\x17\x00\x15\x00\x81\x00\x16\x00\x7c\x00\x21\x00\x4e\x00\x4f\x00\x03\x00\x04\x00\x75\x00\x17\x00\x11\x00\x12\x00\x6c\x00\x18\x00\x19\x00\x7d\x00\x53\x00\x54\x00\x15\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x6a\x00\x18\x00\x19\x00\x11\x00\x12\x00\x17\x00\x3c\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x15\x00\x49\x00\x08\x00\x56\x00\x09\x00\x57\x00\x4a\x00\x0b\x00\x0c\x00\x0d\x00\x17\x00\x7b\x00\x7c\x00\x6e\x00\x39\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x6f\x00\x58\x00\x0e\x00\x0f\x00\x70\x00\xed\xff\x8f\x00\x1a\x00\xee\xff\x1c\x00\x1d\x00\x71\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x3b\x00\x72\x00\x21\x00\x3c\x00\x3d\x00\xff\xff\x21\x00\x3a\x00\xff\xff\x00\x00\x3f\x00\x21\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x00\x00\x46\x00\x47\x00\x48\x00\x56\x00\x24\x00\x57\x00\x00\x00\x26\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x7f\x00\x80\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x1a\x00\x00\x00\x1c\x00\x1d\x00\x56\x00\x00\x00\x57\x00\x00\x00\x00\x00\x07\x00\x03\x00\x04\x00\x08\x00\x00\x00\x09\x00\x86\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x58\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x00\x00\x1c\x00\x1d\x00\x0e\x00\x0f\x00\x3b\x00\x00\x00\x00\x00\x3c\x00\x3d\x00\x00\x00\x3e\x00\x00\x00\x00\x00\x00\x00\x3f\x00\x00\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x00\x00\x46\x00\x47\x00\x48\x00\x3b\x00\x00\x00\x00\x00\x3c\x00\x3d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3f\x00\x00\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x00\x00\x46\x00\x47\x00\x48\x00\x3b\x00\x00\x00\x00\x00\x3c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3f\x00\x00\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x00\x00\x46\x00\x47\x00\x48\x00\x3b\x00\x00\x00\x00\x00\x3c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3f\x00\x00\x00\x00\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x00\x00\x46\x00\x47\x00\x48\x00\x08\x00\x21\x00\x09\x00\x00\x00\x22\x00\x0b\x00\x0c\x00\x0d\x00\x08\x00\x00\x00\x09\x00\x00\x00\x9a\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x08\x00\x00\x00\x09\x00\x00\x00\x86\x00\x0b\x00\x0c\x00\x0d\x00\x08\x00\x00\x00\x09\x00\x00\x00\x88\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x08\x00\x00\x00\x09\x00\x00\x00\x8d\x00\x0b\x00\x0c\x00\x0d\x00\x08\x00\x00\x00\x09\x00\x00\x00\x92\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x08\x00\x00\x00\x09\x00\x00\x00\x79\x00\x0b\x00\x0c\x00\x0d\x00\x08\x00\x00\x00\x09\x00\x00\x00\x64\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x08\x00\x00\x00\x09\x00\x00\x00\x31\x00\x0b\x00\x0c\x00\x0d\x00\x08\x00\x00\x00\x09\x00\x00\x00\x32\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x08\x00\x00\x00\x09\x00\x00\x00\x33\x00\x0b\x00\x0c\x00\x0d\x00\x08\x00\x00\x00\x09\x00\x00\x00\x1d\x00\x0b\x00\x0c\x00\x0d\x00\x08\x00\x00\x00\x09\x00\x0e\x00\x0f\x00\x99\x00\x0c\x00\x0d\x00\x08\x00\x00\x00\x09\x00\x0e\x00\x0f\x00\x96\x00\x0c\x00\x0d\x00\x08\x00\x00\x00\x09\x00\x0e\x00\x0f\x00\x77\x00\x0c\x00\x0d\x00\x08\x00\x00\x00\x09\x00\x0e\x00\x0f\x00\x5a\x00\x0c\x00\x0d\x00\x08\x00\x00\x00\x09\x00\x0e\x00\x0f\x00\x5b\x00\x0c\x00\x0d\x00\x08\x00\x00\x00\x09\x00\x0e\x00\x0f\x00\x5c\x00\x0c\x00\x0d\x00\x08\x00\x00\x00\x09\x00\x0e\x00\x0f\x00\x5d\x00\x0c\x00\x0d\x00\x08\x00\x00\x00\x09\x00\x0e\x00\x0f\x00\x5e\x00\x0c\x00\x0d\x00\x08\x00\x00\x00\x09\x00\x0e\x00\x0f\x00\x5f\x00\x0c\x00\x0d\x00\x08\x00\x00\x00\x09\x00\x0e\x00\x0f\x00\x60\x00\x0c\x00\x0d\x00\x08\x00\x00\x00\x09\x00\x0e\x00\x0f\x00\x61\x00\x0c\x00\x0d\x00\x08\x00\x00\x00\x09\x00\x0e\x00\x0f\x00\x62\x00\x0c\x00\x0d\x00\x08\x00\x00\x00\x09\x00\x0e\x00\x0f\x00\x63\x00\x0c\x00\x0d\x00\x08\x00\x00\x00\x09\x00\x0e\x00\x0f\x00\x65\x00\x0c\x00\x0d\x00\x08\x00\x00\x00\x09\x00\x0e\x00\x0f\x00\x66\x00\x0c\x00\x0d\x00\x08\x00\x00\x00\x09\x00\x0e\x00\x0f\x00\x67\x00\x0c\x00\x0d\x00\x08\x00\x00\x00\x09\x00\x0e\x00\x0f\x00\x68\x00\x0c\x00\x0d\x00\x08\x00\x00\x00\x09\x00\x0e\x00\x0f\x00\x35\x00\x0c\x00\x0d\x00\x00\x00\x3c\x00\x00\x00\x0e\x00\x0f\x00\x00\x00\x00\x00\x7c\x00\x00\x00\x4e\x00\x4f\x00\x0e\x00\x0f\x00\x43\x00\x44\x00\x45\x00\x00\x00\x00\x00\x47\x00\x48\x00\x8f\x00\x53\x00\x54\x00\x90\x00\x4d\x00\x00\x00\x4e\x00\x4f\x00\x00\x00\x00\x00\x4d\x00\x00\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x00\x00\x52\x00\x53\x00\x54\x00\x6c\x00\x51\x00\x00\x00\x52\x00\x53\x00\x54\x00\x4d\x00\x00\x00\x4e\x00\x4f\x00\x00\x00\x4d\x00\x00\x00\x4e\x00\x4f\x00\x00\x00\x72\x00\x51\x00\x00\x00\x52\x00\x53\x00\x54\x00\x91\x00\x00\x00\x52\x00\x53\x00\x54\x00\x7c\x00\x00\x00\x4e\x00\x4f\x00\x7c\x00\x00\x00\x4e\x00\x4f\x00\x7c\x00\x00\x00\x4e\x00\x4f\x00\x00\x00\x89\x00\x53\x00\x54\x00\x00\x00\x8a\x00\x53\x00\x54\x00\x83\x00\x00\x00\x84\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = array (2, 89) [
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89)
	]

happy_n_terms = 46 :: Int
happy_n_nonterms = 24 :: Int

happyReduce_2 = happySpecReduce_1  0# happyReduction_2
happyReduction_2 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn5
		 (happy_var_1
	)}

happyReduce_3 = happySpecReduce_2  0# happyReduction_3
happyReduction_3 happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut6 happy_x_2 of { happy_var_2 -> 
	happyIn5
		 (sL (getLoc happy_var_1) (McEvalS happy_var_1) : happy_var_2
	)}}

happyReduce_4 = happySpecReduce_0  1# happyReduction_4
happyReduction_4  =  happyIn6
		 ([]
	)

happyReduce_5 = happySpecReduce_1  1# happyReduction_5
happyReduction_5 happy_x_1
	 =  happyIn6
		 ([]
	)

happyReduce_6 = happySpecReduce_3  1# happyReduction_6
happyReduction_6 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_2 of { happy_var_2 -> 
	case happyOut6 happy_x_3 of { happy_var_3 -> 
	happyIn6
		 (sL (getLoc happy_var_1) (McEvalS happy_var_2) : happy_var_3
	)}}}

happyReduce_7 = happySpecReduce_3  1# happyReduction_7
happyReduction_7 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut6 happy_x_3 of { happy_var_3 -> 
	happyIn6
		 (happy_var_2 : happy_var_3
	)}}

happyReduce_8 = happySpecReduce_2  1# happyReduction_8
happyReduction_8 happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	case happyOut6 happy_x_2 of { happy_var_2 -> 
	happyIn6
		 (happy_var_1 : happy_var_2
	)}}

happyReduce_9 = happySpecReduce_3  2# happyReduction_9
happyReduction_9 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_2 of { happy_var_2 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	happyIn7
		 (case unLoc happy_var_3 of
             [(L _ McAnyP, exp)] -> sL (comb2 happy_var_1 happy_var_3) (McEvalS exp)
             _ -> sL (comb2 happy_var_1 happy_var_3) (McValueS happy_var_2 (reverse.unLoc $ happy_var_3))
	)}}}

happyReduce_10 = happySpecReduce_1  3# happyReduction_10
happyReduction_10 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (sL (getLoc happy_var_1) (getIDENT $ happy_var_1)
	)}

happyReduce_11 = happySpecReduce_3  3# happyReduction_11
happyReduction_11 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn8
		 (sL (comb2 happy_var_1 happy_var_3) (unLoc happy_var_2)
	)}}}

happyReduce_12 = happySpecReduce_1  4# happyReduction_12
happyReduction_12 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn9
		 (sL (getLoc happy_var_1) (getPREFIX $ happy_var_1)
	)}

happyReduce_13 = happySpecReduce_1  4# happyReduction_13
happyReduction_13 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn9
		 (sL (getLoc happy_var_1) (getINFIX0 $ happy_var_1)
	)}

happyReduce_14 = happySpecReduce_1  4# happyReduction_14
happyReduction_14 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn9
		 (sL (getLoc happy_var_1) (getINFIX1 $ happy_var_1)
	)}

happyReduce_15 = happySpecReduce_1  4# happyReduction_15
happyReduction_15 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn9
		 (sL (getLoc happy_var_1) (getINFIX2 $ happy_var_1)
	)}

happyReduce_16 = happySpecReduce_1  4# happyReduction_16
happyReduction_16 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn9
		 (sL (getLoc happy_var_1) (mkName "+")
	)}

happyReduce_17 = happySpecReduce_1  4# happyReduction_17
happyReduction_17 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn9
		 (sL (getLoc happy_var_1) (mkName "-")
	)}

happyReduce_18 = happySpecReduce_1  4# happyReduction_18
happyReduction_18 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn9
		 (sL (getLoc happy_var_1) (mkName "-")
	)}

happyReduce_19 = happySpecReduce_1  4# happyReduction_19
happyReduction_19 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn9
		 (sL (getLoc happy_var_1) (mkName "*")
	)}

happyReduce_20 = happySpecReduce_1  4# happyReduction_20
happyReduction_20 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn9
		 (sL (getLoc happy_var_1) (mkName "=")
	)}

happyReduce_21 = happySpecReduce_1  4# happyReduction_21
happyReduction_21 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn9
		 (sL (getLoc happy_var_1) (mkName "<")
	)}

happyReduce_22 = happySpecReduce_1  4# happyReduction_22
happyReduction_22 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn9
		 (sL (getLoc happy_var_1) (mkName ">")
	)}

happyReduce_23 = happySpecReduce_1  4# happyReduction_23
happyReduction_23 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn9
		 (sL (getLoc happy_var_1) (mkName "||")
	)}

happyReduce_24 = happySpecReduce_1  4# happyReduction_24
happyReduction_24 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn9
		 (sL (getLoc happy_var_1) (mkName "&&")
	)}

happyReduce_25 = happySpecReduce_1  5# happyReduction_25
happyReduction_25 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (sL (getLoc happy_var_1) (IntConst (getINT happy_var_1))
	)}

happyReduce_26 = happySpecReduce_1  5# happyReduction_26
happyReduction_26 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (sL (getLoc happy_var_1) (FloatConst (getFLOAT happy_var_1))
	)}

happyReduce_27 = happySpecReduce_1  6# happyReduction_27
happyReduction_27 happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	happyIn11
		 (happy_var_1
	)}

happyReduce_28 = happySpecReduce_2  6# happyReduction_28
happyReduction_28 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	happyIn11
		 (sL (comb2 happy_var_1 happy_var_2) (IntConst (- (getINT happy_var_2)))
	)}}

happyReduce_29 = happySpecReduce_2  6# happyReduction_29
happyReduction_29 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	happyIn11
		 (sL (comb2 happy_var_1 happy_var_2) (FloatConst (- (getFLOAT happy_var_2)))
	)}}

happyReduce_30 = happySpecReduce_1  7# happyReduction_30
happyReduction_30 happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	happyIn12
		 (happy_var_1
	)}

happyReduce_31 = happySpecReduce_2  7# happyReduction_31
happyReduction_31 happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	happyIn12
		 (sL (comb2 happy_var_1 happy_var_2) (unLoc happy_var_1)
	)}}

happyReduce_32 = happySpecReduce_3  7# happyReduction_32
happyReduction_32 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn12
		 (sL (comb2 happy_var_1 happy_var_3) (McSeqE happy_var_1 happy_var_3)
	)}}

happyReduce_33 = happySpecReduce_1  8# happyReduction_33
happyReduction_33 happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	happyIn13
		 (happy_var_1
	)}

happyReduce_34 = happySpecReduce_2  8# happyReduction_34
happyReduction_34 happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_2 of { happy_var_2 -> 
	happyIn13
		 (sL (comb2 happy_var_1 happy_var_2) (McAppE happy_var_1 (reverse (unLoc happy_var_2)))
	)}}

happyReduce_35 = happyReduce 5# 8# happyReduction_35
happyReduction_35 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_2 of { happy_var_2 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	case happyOut12 happy_x_5 of { happy_var_5 -> 
	happyIn13
		 (sL (comb2 happy_var_1 happy_var_5) (McLetE happy_var_2 (reverse (unLoc happy_var_3)) happy_var_5)
	) `HappyStk` happyRest}}}}

happyReduce_36 = happyReduce 5# 8# happyReduction_36
happyReduction_36 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_2 of { happy_var_2 -> 
	case happyOut24 happy_x_5 of { happy_var_5 -> 
	happyIn13
		 (sL (comb2 happy_var_1 happy_var_5) (McMatchE happy_var_2 (reverse (unLoc happy_var_5)))
	) `HappyStk` happyRest}}}

happyReduce_37 = happySpecReduce_1  8# happyReduction_37
happyReduction_37 happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	happyIn13
		 (sL (getLoc happy_var_1) (McTupleE . reverse . unLoc $ happy_var_1)
	)}

happyReduce_38 = happyReduce 6# 8# happyReduction_38
happyReduction_38 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_2 of { happy_var_2 -> 
	case happyOut13 happy_x_4 of { happy_var_4 -> 
	case happyOut13 happy_x_6 of { happy_var_6 -> 
	happyIn13
		 (sL (comb2 happy_var_1 happy_var_6) (McCondE happy_var_2 happy_var_4 happy_var_6)
	) `HappyStk` happyRest}}}}

happyReduce_39 = happySpecReduce_3  8# happyReduction_39
happyReduction_39 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (sL (comb2 happy_var_1 happy_var_3) (mkInfix0 happy_var_1 happy_var_2 happy_var_3)
	)}}}

happyReduce_40 = happySpecReduce_3  8# happyReduction_40
happyReduction_40 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (sL (comb2 happy_var_1 happy_var_3) (mkInfix1 happy_var_1 happy_var_2 happy_var_3)
	)}}}

happyReduce_41 = happySpecReduce_3  8# happyReduction_41
happyReduction_41 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (sL (comb2 happy_var_1 happy_var_3) (mkInfix2 happy_var_1 happy_var_2 happy_var_3)
	)}}}

happyReduce_42 = happySpecReduce_3  8# happyReduction_42
happyReduction_42 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (sL (comb2 happy_var_1 happy_var_3) (mkInfix happy_var_1 (getLoc happy_var_2, "+") happy_var_3)
	)}}}

happyReduce_43 = happySpecReduce_3  8# happyReduction_43
happyReduction_43 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (sL (comb2 happy_var_1 happy_var_3) (mkInfix happy_var_1 (getLoc happy_var_2, "-") happy_var_3)
	)}}}

happyReduce_44 = happySpecReduce_3  8# happyReduction_44
happyReduction_44 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (sL (comb2 happy_var_1 happy_var_3) (mkInfix happy_var_1 (getLoc happy_var_2, "-.") happy_var_3)
	)}}}

happyReduce_45 = happySpecReduce_3  8# happyReduction_45
happyReduction_45 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (sL (comb2 happy_var_1 happy_var_3) (mkInfix happy_var_1 (getLoc happy_var_2, "*") happy_var_3)
	)}}}

happyReduce_46 = happySpecReduce_3  8# happyReduction_46
happyReduction_46 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (sL (comb2 happy_var_1 happy_var_3) (mkInfix happy_var_1 (getLoc happy_var_2, "=") happy_var_3)
	)}}}

happyReduce_47 = happySpecReduce_3  8# happyReduction_47
happyReduction_47 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (sL (comb2 happy_var_1 happy_var_3) (mkInfix happy_var_1 (getLoc happy_var_2, "<") happy_var_3)
	)}}}

happyReduce_48 = happySpecReduce_3  8# happyReduction_48
happyReduction_48 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (sL (comb2 happy_var_1 happy_var_3) (mkInfix happy_var_1 (getLoc happy_var_2, ">") happy_var_3)
	)}}}

happyReduce_49 = happySpecReduce_3  8# happyReduction_49
happyReduction_49 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (sL (comb2 happy_var_1 happy_var_3) (mkInfix happy_var_1 (getLoc happy_var_2, "||") happy_var_3)
	)}}}

happyReduce_50 = happySpecReduce_3  8# happyReduction_50
happyReduction_50 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (sL (comb2 happy_var_1 happy_var_3) (mkInfix happy_var_1 (getLoc happy_var_2, "&&") happy_var_3)
	)}}}

happyReduce_51 = happySpecReduce_2  8# happyReduction_51
happyReduction_51 happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_2 of { happy_var_2 -> 
	happyIn13
		 (sL (comb2 happy_var_1 happy_var_2) (mkUnaryMinus happy_var_1 happy_var_2)
	)}}

happyReduce_52 = happyReduce 7# 8# happyReduction_52
happyReduction_52 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut15 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_4 of { happy_var_4 -> 
	case happyOut13 happy_x_7 of { happy_var_7 -> 
	happyIn13
		 (sL (comb2 happy_var_1 happy_var_7) 
            (McAppE (L noSrcLoc (McVarE arraySetName)) [happy_var_1, happy_var_4, happy_var_7])
	) `HappyStk` happyRest}}}

happyReduce_53 = happySpecReduce_3  9# happyReduction_53
happyReduction_53 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn14
		 (sL (comb2 happy_var_1 happy_var_3) (happy_var_3 : (unLoc happy_var_1))
	)}}

happyReduce_54 = happySpecReduce_3  9# happyReduction_54
happyReduction_54 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn14
		 (sL (comb2 happy_var_1 happy_var_3) [happy_var_3, happy_var_1]
	)}}

happyReduce_55 = happySpecReduce_1  10# happyReduction_55
happyReduction_55 happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	happyIn15
		 (sL (getLoc happy_var_1) (McVarE (unLoc happy_var_1))
	)}

happyReduce_56 = happySpecReduce_1  10# happyReduction_56
happyReduction_56 happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	happyIn15
		 (sL (getLoc happy_var_1) (McLitE (unLoc happy_var_1))
	)}

happyReduce_57 = happySpecReduce_1  10# happyReduction_57
happyReduction_57 happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	happyIn15
		 (sL (getLoc happy_var_1) (McConsE happy_var_1 Nothing)
	)}

happyReduce_58 = happySpecReduce_3  10# happyReduction_58
happyReduction_58 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn15
		 (sL (comb2 happy_var_1 happy_var_3) (unLoc happy_var_2)
	)}}}

happyReduce_59 = happySpecReduce_3  10# happyReduction_59
happyReduction_59 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn15
		 (sL (comb2 happy_var_1 happy_var_3) (unLoc happy_var_2)
	)}}}

happyReduce_60 = happyReduce 5# 10# happyReduction_60
happyReduction_60 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut15 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_4 of { happy_var_4 -> 
	case happyOutTok happy_x_5 of { happy_var_5 -> 
	happyIn15
		 (sL (comb2 happy_var_1 happy_var_5) 
            (McAppE (L noSrcLoc (McVarE arrayGetName)) [happy_var_1, happy_var_4])
	) `HappyStk` happyRest}}}

happyReduce_61 = happySpecReduce_1  11# happyReduction_61
happyReduction_61 happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	happyIn16
		 (sL (getLoc happy_var_1) [ happy_var_1 ]
	)}

happyReduce_62 = happySpecReduce_2  11# happyReduction_62
happyReduction_62 happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut15 happy_x_2 of { happy_var_2 -> 
	happyIn16
		 (sL (comb2 happy_var_1 happy_var_2) (happy_var_2 : (unLoc happy_var_1))
	)}}

happyReduce_63 = happySpecReduce_0  12# happyReduction_63
happyReduction_63  =  happyIn17
		 (NonRec
	)

happyReduce_64 = happySpecReduce_1  12# happyReduction_64
happyReduction_64 happy_x_1
	 =  happyIn17
		 (Rec
	)

happyReduce_65 = happySpecReduce_1  13# happyReduction_65
happyReduction_65 happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	happyIn18
		 (sL (getLoc happy_var_1) [unLoc happy_var_1]
	)}

happyReduce_66 = happySpecReduce_3  13# happyReduction_66
happyReduction_66 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut19 happy_x_3 of { happy_var_3 -> 
	happyIn18
		 (sL (comb2 happy_var_1 happy_var_3) ((unLoc happy_var_3) : (unLoc happy_var_1))
	)}}

happyReduce_67 = happySpecReduce_2  14# happyReduction_67
happyReduction_67 happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut20 happy_x_2 of { happy_var_2 -> 
	happyIn19
		 (sL (comb2 happy_var_1 happy_var_2) (mkVarP happy_var_1, happy_var_2)
	)}}

happyReduce_68 = happySpecReduce_3  14# happyReduction_68
happyReduction_68 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn19
		 (sL (comb2 happy_var_1 happy_var_3) (happy_var_1, happy_var_3)
	)}}

happyReduce_69 = happySpecReduce_2  15# happyReduction_69
happyReduction_69 happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_2 of { happy_var_2 -> 
	happyIn20
		 (happy_var_2
	)}

happyReduce_70 = happySpecReduce_2  15# happyReduction_70
happyReduction_70 happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	case happyOut20 happy_x_2 of { happy_var_2 -> 
	happyIn20
		 (sL (comb2 happy_var_1 happy_var_2) (McFunE happy_var_1 happy_var_2)
	)}}

happyReduce_71 = happySpecReduce_1  16# happyReduction_71
happyReduction_71 happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	happyIn21
		 (happy_var_1
	)}

happyReduce_72 = happySpecReduce_1  16# happyReduction_72
happyReduction_72 happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	happyIn21
		 (sL (getLoc happy_var_1) (McTupleP (reverse (unLoc happy_var_1)))
	)}

happyReduce_73 = happySpecReduce_1  17# happyReduction_73
happyReduction_73 happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	happyIn22
		 (mkVarP happy_var_1
	)}

happyReduce_74 = happySpecReduce_1  17# happyReduction_74
happyReduction_74 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn22
		 (sL (getLoc happy_var_1) McAnyP
	)}

happyReduce_75 = happySpecReduce_1  17# happyReduction_75
happyReduction_75 happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	happyIn22
		 (sL (getLoc happy_var_1) (McLitP (unLoc happy_var_1))
	)}

happyReduce_76 = happySpecReduce_3  17# happyReduction_76
happyReduction_76 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn22
		 (sL (comb2 happy_var_1 happy_var_3) (unLoc happy_var_2)
	)}}}

happyReduce_77 = happySpecReduce_3  18# happyReduction_77
happyReduction_77 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 (sL (comb2 happy_var_1 happy_var_3) (happy_var_3 : (unLoc happy_var_1))
	)}}

happyReduce_78 = happySpecReduce_3  18# happyReduction_78
happyReduction_78 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 (sL (comb2 happy_var_1 happy_var_3) [happy_var_3, happy_var_1]
	)}}

happyReduce_79 = happySpecReduce_2  19# happyReduction_79
happyReduction_79 happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_2 of { happy_var_2 -> 
	happyIn24
		 (sL (getLoc happy_var_1) [(happy_var_1, happy_var_2)]
	)}}

happyReduce_80 = happyReduce 4# 19# happyReduction_80
happyReduction_80 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut24 happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_3 of { happy_var_3 -> 
	case happyOut25 happy_x_4 of { happy_var_4 -> 
	happyIn24
		 (sL (comb2 happy_var_1 happy_var_4) ((happy_var_3, happy_var_4) : (unLoc happy_var_1))
	) `HappyStk` happyRest}}}

happyReduce_81 = happySpecReduce_2  20# happyReduction_81
happyReduction_81 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_2 of { happy_var_2 -> 
	happyIn25
		 (sL (comb2 happy_var_1 happy_var_2) (unLoc happy_var_2)
	)}}

happyReduce_82 = happySpecReduce_0  21# happyReduction_82
happyReduction_82  =  happyIn26
		 (()
	)

happyReduce_83 = happySpecReduce_1  21# happyReduction_83
happyReduction_83 happy_x_1
	 =  happyIn26
		 (()
	)

happyReduce_84 = happySpecReduce_1  22# happyReduction_84
happyReduction_84 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn27
		 (sL (getLoc happy_var_1) "-"
	)}

happyReduce_85 = happySpecReduce_1  22# happyReduction_85
happyReduction_85 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn27
		 (sL (getLoc happy_var_1) "-."
	)}

happyReduce_86 = happySpecReduce_1  23# happyReduction_86
happyReduction_86 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn28
		 (sL (getLoc happy_var_1) (getUIDENT happy_var_1)
	)}

happyReduce_87 = happySpecReduce_2  23# happyReduction_87
happyReduction_87 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	happyIn28
		 (sL (comb2 happy_var_1 happy_var_2) (mkName "()")
	)}}

happyReduce_88 = happySpecReduce_1  23# happyReduction_88
happyReduction_88 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn28
		 (sL (getLoc happy_var_1) (mkName "true")
	)}

happyReduce_89 = happySpecReduce_1  23# happyReduction_89
happyReduction_89 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn28
		 (sL (getLoc happy_var_1) (mkName "false")
	)}

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = happyDoAction i tk action sts stk in
	case tk of {
	L _ Teof -> happyDoAction 45# tk action sts stk;
	L _ Ttrue -> cont 1#;
	L _ Tfalse -> cont 2#;
	L _ Tlet -> cont 3#;
	L _ Trec -> cont 4#;
	L _ Tin -> cont 5#;
	L _ Tif -> cont 6#;
	L _ Tthen -> cont 7#;
	L _ Telse -> cont 8#;
	L _ Tbegin -> cont 9#;
	L _ Tend -> cont 10#;
	L _ Tmatch -> cont 11#;
	L _ Twith -> cont 12#;
	L _ Ttype -> cont 13#;
	L _ Tof -> cont 14#;
	L _ Tmod -> cont 15#;
	L _ Tand -> cont 16#;
	L _ Tunderscore -> cont 17#;
	L _ Tampamp -> cont 18#;
	L _ Tlparen -> cont 19#;
	L _ Trparen -> cont 20#;
	L _ Tstar -> cont 21#;
	L _ Tcomma -> cont 22#;
	L _ Tdot -> cont 23#;
	L _ Tsemi -> cont 24#;
	L _ Tsemisemi -> cont 25#;
	L _ Tlessminus -> cont 26#;
	L _ Tminusgreater -> cont 27#;
	L _ Tequal -> cont 28#;
	L _ Tbar -> cont 29#;
	L _ Tbarbar -> cont 30#;
	L _ Tless -> cont 31#;
	L _ Tgreater -> cont 32#;
	L _ Tplus -> cont 33#;
	L _ Tminus -> cont 34#;
	L _ Tminusdot -> cont 35#;
	L _ (Tprefix _) -> cont 36#;
	L _ (Tinfix0 _) -> cont 37#;
	L _ (Tinfix1 _) -> cont 38#;
	L _ (Tinfix2 _) -> cont 39#;
	L _ (Tident _) -> cont 40#;
	L _ (Tuident _) -> cont 41#;
	L _ (Tint _) -> cont 42#;
	L _ (Tfloat _) -> cont 43#;
	L _ Teof -> cont 44#;
	_ -> happyError' tk
	})

happyError_ tk = happyError' tk

happyThen :: () => P a -> (a -> P b) -> P b
happyThen = (>>=)
happyReturn :: () => a -> P a
happyReturn = (return)
happyThen1 = happyThen
happyReturn1 :: () => a -> P a
happyReturn1 = happyReturn
happyError' :: () => Located Token -> P a
happyError' tk = (\token -> happyError) tk

parseExpr = happySomeParser where
  happySomeParser = happyThen (happyParse 0#) (\x -> happyReturn (happyOut12 x))

parse = happySomeParser where
  happySomeParser = happyThen (happyParse 1#) (\x -> happyReturn (happyOut5 x))

happySeq = happyDontSeq


arraySetName = mkName "array_set"
arrayGetName = mkName "array_get"

getIDENT  (L _ (Tident x))  = x
getUIDENT (L _ (Tuident x)) = x
getPREFIX (L _ (Tprefix x)) = x
getINFIX0 (L _ (Tinfix0 x)) = x
getINFIX1 (L _ (Tinfix1 x)) = x
getINFIX2 (L _ (Tinfix2 x)) = x
getINT    (L _ (Tint x))    = x
getFLOAT  (L _ (Tfloat x))  = x

mkVarE (L loc name) = sL loc (McVarE name)
mkVarP (L loc name) = sL loc (McVarP name)

mkUnaryMinus (L _ "-") (L _ (McLitE (IntConst n))) 
    = McLitE (IntConst (-n))
mkUnaryMinus (L _ _) (L _ (McLitE (FloatConst f)))
    = McLitE (FloatConst (-f))
mkUnaryMinus (L loc op) expr
    = McAppE (L loc (McVarE . mkName $ '~':op)) [expr]

mkInfix0 arg1 op arg2 =
    McAppE (sL (getLoc op) (mkOpVar.unName.getINFIX0 $ op)) [arg1, arg2]
mkInfix1 arg1 op arg2 =
    McAppE (sL (getLoc op) (mkOpVar.unName.getINFIX1 $ op)) [arg1, arg2]
mkInfix2 arg1 op arg2 =
    McAppE (sL (getLoc op) (mkOpVar.unName.getINFIX2 $ op)) [arg1, arg2]
mkInfix arg1 (loc, op) arg2 =
    McAppE (sL loc (mkOpVar op)) [arg1, arg2] 

mkOpVar op = McVarE . mkName $ "(" ++ op ++ ")"

reLoc (L loc _) (L _ e) = sL loc e

comb2 :: Located a -> Located b -> SrcLoc
comb2 a b = combineSrcLoc (getLoc a) (getLoc b)


happyError :: P a
happyError = fail "parse error"
{-# LINE 1 "GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command line>" #-}
{-# LINE 1 "GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 28 "GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Int# Happy_IntList





{-# LINE 49 "GenericTemplate.hs" #-}

{-# LINE 59 "GenericTemplate.hs" #-}

{-# LINE 68 "GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	(happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
	= {- nothing -}


	  case action of
		0#		  -> {- nothing -}
				     happyFail i tk st
		-1# 	  -> {- nothing -}
				     happyAccept i tk st
		n | (n <# (0# :: Int#)) -> {- nothing -}

				     (happyReduceArr ! rule) i tk st
				     where rule = (I# ((negateInt# ((n +# (1# :: Int#))))))
		n		  -> {- nothing -}


				     happyShift new_state i tk st
				     where new_state = (n -# (1# :: Int#))
   where off    = indexShortOffAddr happyActOffsets st
	 off_i  = (off +# i)
	 check  = if (off_i >=# (0# :: Int#))
			then (indexShortOffAddr happyCheck off_i ==#  i)
			else False
 	 action | check     = indexShortOffAddr happyTable off_i
		| otherwise = indexShortOffAddr happyDefActions st

{-# LINE 127 "GenericTemplate.hs" #-}


indexShortOffAddr (HappyA# arr) off =
#if __GLASGOW_HASKELL__ > 500
	narrow16Int# i
#elif __GLASGOW_HASKELL__ == 500
	intToInt16# i
#else
	(i `iShiftL#` 16#) `iShiftRA#` 16#
#endif
  where
#if __GLASGOW_HASKELL__ >= 503
	i = word2Int# ((high `uncheckedShiftL#` 8#) `or#` low)
#else
	i = word2Int# ((high `shiftL#` 8#) `or#` low)
#endif
	high = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
	low  = int2Word# (ord# (indexCharOffAddr# arr off'))
	off' = off *# 2#





data HappyAddr = HappyA# Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 170 "GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case unsafeCoerce# x of { (I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k -# (1# :: Int#)) sts of
	 sts1@((HappyCons (st1@(action)) (_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@((HappyCons (st1@(action)) (_))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where sts1@((HappyCons (st1@(action)) (_))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

             off    = indexShortOffAddr happyGotoOffsets st1
             off_i  = (off +# nt)
             new_state = indexShortOffAddr happyTable off_i




happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n -# (1# :: Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n -# (1#::Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off    = indexShortOffAddr happyGotoOffsets st
	 off_i  = (off +# nt)
 	 new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail  0# tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
	happyDoAction 0# tk action sts ( (unsafeCoerce# (I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.

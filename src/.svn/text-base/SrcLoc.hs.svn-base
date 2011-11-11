------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module SrcLoc (
    SrcLoc, -- abstract
    sL, mkSrcLoc, noSrcLoc,
    advanceSrcLoc, startSrcLoc, combineSrcLoc,
    Located(..),
    unLoc, getLoc
    ) where

import Outputable
import Panic

{- source location -}

data SrcLoc
    = SrcLoc {
        srcLocLine  :: !Int,    -- line number, 1 origin
        srcLocCol   :: !Int     -- column number, 0 origin
        }
    | SrcSpan {
        srcSpanSLine    :: !Int,    -- start line
        srcSpanSCol     :: !Int,    -- start column
        srcSpanELine    :: !Int,    -- end line
        srcSpanECol     :: !Int     -- end column
        }
    | UnhelpfulLoc String
    deriving (Eq)

{-# INLINE sL #-}
sL :: SrcLoc -> a -> Located a
sL loc a = loc `seq` L loc a

mkSrcLoc line col = SrcLoc line col
noSrcLoc = UnhelpfulLoc "<no location info>"

advanceSrcLoc :: SrcLoc -> Char -> SrcLoc
advanceSrcLoc (SrcLoc l _) '\n' = SrcLoc (l+1) 0
advanceSrcLoc (SrcLoc l c) _    = SrcLoc l (c+1)
advanceSrcLoc loc _ = loc

startSrcLoc = mkSrcLoc 1 0

combineSrcLoc :: SrcLoc -> SrcLoc -> SrcLoc
combineSrcLoc (SrcLoc sl sc) (SrcLoc el ec)
    = SrcSpan sl sc el ec
combineSrcLoc (SrcLoc sl sc) (SrcSpan _ _ el ec)
    = SrcSpan sl sc el ec
combineSrcLoc (SrcSpan sl sc _ _) (SrcLoc el ec)
    = SrcSpan sl sc el ec
combineSrcLoc (SrcSpan sl sc _ _) (SrcSpan _ _ el ec)
    = SrcSpan sl sc el ec
combineSrcLoc _ _ = panic "combineSrcLoc"

instance Outputable SrcLoc where
    ppr (SrcLoc l c) = ppr (l,c)
    ppr (SrcSpan sl sc el ec) = ppr (sl,sc) <> char '-' <> ppr (el,ec)
    ppr (UnhelpfulLoc str) = text str

instance Show SrcLoc where
    show l = show $ ppr l

------------------------------------------------------------
--  The location info wrapper
------------------------------------------------------------

data Located e = L SrcLoc e
    deriving (Eq)

unLoc (L _ e) = e
getLoc (L loc _) = loc


instance (Outputable e) => Outputable (Located e) where
    ppr = ppr.unLoc

instance (Show e) => Show (Located e) where
    show = show.unLoc

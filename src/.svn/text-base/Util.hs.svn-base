module Util (
  catchMaybe 
  ) where

catchMaybe :: Monad m => m (Maybe a) -> m a -> m a
catchMaybe m f
  = do x <- m
       case x of
          Just x' -> return x'
          Nothing -> f

module HTMLTokenizer.MonadPlus
where

import HTMLTokenizer.Prelude hiding (foldl, foldl1, concat)


{-# INLINE foldl #-}
foldl :: MonadPlus m => (a -> b -> a) -> a -> m b -> m a
foldl step start fetch =
  loop start
  where
    loop !state =
      mplus
        (do
          !element <- fetch
          loop (step state element))
        (return state)

{-# INLINE foldl1 #-}
foldl1 :: MonadPlus m => (a -> a -> a) -> m a -> m a
foldl1 step fetch =
  do
    !start <- fetch
    foldl step start fetch

{-# INLINE concat #-}
concat :: (MonadPlus m, Monoid a) => m a -> m a
concat =
  foldl mappend mempty

{-# INLINE concat1 #-}
concat1 :: (MonadPlus m, Semigroup a) => m a -> m a
concat1 =
  foldl1 (<>)

{-# INLINE foldlM #-}
foldlM :: MonadPlus m => (a -> b -> m a) -> a -> m b -> m a
foldlM step start fetch =
  loop start
  where
    loop !state =
      mplus
        (do
          !element <- fetch
          loop =<< step state element)
        (return state)

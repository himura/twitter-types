{-# LANGUAGE RankNTypes #-}

module Web.Twitter.Types.Lens.Types
       ( Lens
       , SimpleLens
       ) where

-- | A type alias of the lens.
-- It is the same definition as the lens which introduced in
-- <http://hackage.haskell.org/package/lens-4.3.3/docs/Control-Lens-Type.html#t:Lens>
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- | Same as Lens' <http://hackage.haskell.org/package/lens-4.3.3/docs/Control-Lens-Type.html#t:Lens-39->
--
-- The type name with ' letter is not works well with clang cpp preprocessor,
-- so we use "SimpleLens" instead of Lens'.
type SimpleLens s a = Lens s s a a

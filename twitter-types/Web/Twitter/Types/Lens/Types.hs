{-# LANGUAGE RankNTypes #-}

module Web.Twitter.Types.Lens.Types
       ( Lens
       , Lens'
       ) where

-- | A type alias of the lens.
-- It is the same definition as the lens which introduced in
-- <http://hackage.haskell.org/package/lens-4.3.3/docs/Control-Lens-Type.html#t:Lens>
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- | Same as Lens' <http://hackage.haskell.org/package/lens-4.3.3/docs/Control-Lens-Type.html#t:Lens-39->
type Lens' s a = Lens s s a a

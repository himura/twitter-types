{-# LANGUAGE RankNTypes #-}

module Web.Twitter.Types.Lens.Types
       ( Lens
       , SimpleLens
       ) where

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type SimpleLens s a = Lens s s a a

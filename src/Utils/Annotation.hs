{-# LANGUAGE DeriveFunctor #-}

module Utils.Annotation where

data Annotation a b = Ann a b deriving (Show, Eq, Functor)

annotation :: Annotation a b -> a
annotation (Ann a _) = a

value :: Annotation a b -> b
value (Ann _ b) = b
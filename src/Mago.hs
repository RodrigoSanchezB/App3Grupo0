module Mago (Mago(..), Coordenada) where

type Coordenada = (Int, Int)

data Mago = Mago
  { pos :: Coordenada
  , energia :: Int
  , visitados :: [Coordenada]
  } deriving (Show)

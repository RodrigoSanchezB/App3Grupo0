module Mago where

type Coordenada = (Int, Int)

data Mago = Mago
  { posicion :: Coordenada
  , energia :: Int
  , camino :: [Coordenada]
  } deriving (Show, Eq)
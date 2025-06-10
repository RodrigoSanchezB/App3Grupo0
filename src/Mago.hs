module Mago where

type Coordenada = (Int, Int)

data Mago = Mago
  { posicion :: Coordenada
  , energia :: Int
  , visitadas :: [Coordenada]
  } deriving (Show, Eq)

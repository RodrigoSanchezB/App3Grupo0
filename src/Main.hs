module Main where

import Mago
import Bosque

bosqueEjemplo :: Bosque
bosqueEjemplo =
  [ [ 2, -3,  1,  0,  2,  3]
  , [-5,  4, -2,  1,  0, -4]
  , [ 1,  3,  0, -3,  2,  2]
  , [ 2, -1,  4,  0, -5,  1]
  , [ 0,  2, -3,  3,  4, -1]
  , [ 1,  0,  2, -2,  1,  5]
  ]

main :: IO ()
main = do
  let energiaInicial = 12
  case buscarMejorCamino bosqueEjemplo energiaInicial of
    Nothing -> putStrLn "No hay camino válido"
    Just camino -> do
      putStrLn "Mejor camino (coordenada, energía):"
      mapM_ print camino
      putStrLn $ "Energía final: " ++ show (snd $ last camino)

module Main where

import Bosque
import Mago
import Data.List (maximumBy)
import Data.Ord (comparing)

bosqueEjemplo :: [[Int]]
bosqueEjemplo =
  [ [ 2, -3, 1, 0, 2, 3]
  , [-5, 4, -2, 1, 0, -4]
  , [ 1, 3, 0, -3, 2, 2]
  , [ 2, -1, 4, 0, -5, 1]
  , [ 0, 2, -3, 3, 4, -1]
  , [ 1, 0, 2, -2, 1, 5]
  ]

energiaInicial :: Int
energiaInicial = 12

main :: IO ()
main = do
  let caminos = buscarCaminos bosqueEjemplo (Mago (0, 0) (energiaInicial + valorCelda bosqueEjemplo (0,0)) [])
  if null caminos
    then putStrLn "No hay caminos válidos"
    else do
      let mejor = maximumBy (comparing (snd . last)) caminos
      putStrLn "Mejor camino:"
      print $ map fst mejor
      putStrLn $ "Energía final: " ++ show (snd $ last mejor)
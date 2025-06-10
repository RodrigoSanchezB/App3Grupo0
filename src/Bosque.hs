module Bosque where

import Mago
import Data.List (maximumBy)
import Data.Ord (comparing)

type Bosque = [[Int]]

-- Verifica si una coordenada está dentro de los límites del bosque
dentroLimites :: Bosque -> Coordenada -> Bool
dentroLimites bosque (x, y) =
  x >= 0 && y >= 0 && x < length bosque && y < length (head bosque)

-- Devuelve el valor de una celda
valorCelda :: Bosque -> Coordenada -> Int
valorCelda bosque (x, y) = (bosque !! x) !! y

-- Lista de movimientos posibles desde una posición (sin repetir)
movimientosValidos :: Bosque -> [Coordenada] -> Coordenada -> [Coordenada]
movimientosValidos bosque visitadas (x, y) =
  filter (\c -> dentroLimites bosque c && notElem c visitadas)
    [ (x+1, y)       -- abajo
    , (x, y+1)       -- derecha
    , (x+1, y+1)     -- diagonal
    , (x-1, y)       -- arriba
    , (x, y-1)       -- izquierda
    ]

-- Calcula nueva energía incluyendo penalizaciones
energiaNueva :: Int -> Int -> Coordenada -> Coordenada -> Int
energiaNueva actual valor origen destino =
  let penalizacionDiagonal = if destino == (fst origen + 1, snd origen + 1) then 2 else 0
      penalizacionTrampa = if valor == 0 then 3 else 0
  in actual + valor - penalizacionDiagonal - penalizacionTrampa

-- Función principal: explorar caminos
buscarCaminos :: Bosque -> Mago -> [[(Coordenada, Int)]]
buscarCaminos bosque (Mago pos e cam)
  | pos == destino && e >= 0 = [zip (reverse (pos : cam)) (repeat e)]
  | e < 0 = []
  | otherwise =
      let vecinos = movimientosValidos bosque (pos:cam) pos
      in concatMap (\p ->
            let v = valorCelda bosque p
                nuevaE = energiaNueva e v pos p
            in buscarCaminos bosque (Mago p nuevaE (pos:cam))
         ) vecinos
  where
    destino = (length bosque - 1, length (head bosque) - 1)
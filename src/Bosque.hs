module Bosque where

import Mago
import Data.List (maximumBy)
import Data.Ord (comparing)
import qualified Data.Map as Map
import Debug.Trace

type Bosque = [[Int]]
type Memo = Map.Map (Coordenada, Int) (Maybe [(Coordenada, Int)])

-- Verifica si una coordenada está dentro de los límites del bosque
dentroLimites :: Bosque -> Coordenada -> Bool
dentroLimites bosque (x, y) =
  x >= 0 && y >= 0 && x < length bosque && not (null bosque) && y < length (bosque !! 0)

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

-- Función principal con memoization y trazas
buscarMejorCamino :: Bosque -> Mago -> [(Coordenada, Int)]
buscarMejorCamino bosque mago =
  case buscarMejorCamino' bosque mago Map.empty of
    (Just camino, _) -> camino
    (Nothing, _) -> []

-- Versión auxiliar con memo
buscarMejorCamino' :: Bosque -> Mago -> Memo -> (Maybe [(Coordenada, Int)], Memo)
buscarMejorCamino' bosque (Mago pos e cam) memo
  | e < 0 = (Nothing, memo)
  | pos == destino = 
      let res = Just (reverse ((pos, e) : cam))  -- Aquí se agrega la posición actual con energía
      in (res, Map.insert (pos, e) res memo)
  | otherwise =
      case Map.lookup (pos, e) memo of
        Just res -> (res, memo)
        Nothing ->
          let vecinos = movimientosValidos bosque (map fst cam ++ [pos]) pos
              (caminos, memoFinal) = foldl procesar ([], memo) vecinos
              mejor = if null caminos then Nothing else Just (maximumBy (comparing (sum . map snd)) caminos)
              memoNuevo = Map.insert (pos, e) mejor memoFinal
          in (mejor, memoNuevo)
  where
    destino = (length bosque - 1, length (head bosque) - 1)

    procesar (acc, m) p =
      let v = valorCelda bosque p
          nuevaE = energiaNueva e v pos p
          magoNuevo = Mago p nuevaE ((pos, e) : cam)
          (res, m2) = buscarMejorCamino' bosque magoNuevo m
      in trace ("Visitando: " ++ show p ++ " con energía: " ++ show nuevaE) $
         case res of
           Just camino -> (camino : acc, m2)
           Nothing -> (acc, m2)

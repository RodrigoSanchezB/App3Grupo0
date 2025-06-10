module Bosque where

import Mago
import qualified Data.Map as Map

type Bosque = [[Int]]
type Memo = Map.Map (Coordenada, Int) (Maybe [(Coordenada, Int)])

-- Verifica si una coordenada está dentro de los límites del bosque
dentroLimites :: Bosque -> Coordenada -> Bool
dentroLimites bosque (x, y) =
  x >= 0 && y >= 0 && x < length bosque && y < length (head bosque)

-- Valor de la celda
valorCelda :: Bosque -> Coordenada -> Int
valorCelda bosque (x, y) = (bosque !! x) !! y

-- Movimientos posibles, sin volver a visitadas para arriba e izquierda
movimientosValidos :: Bosque -> [Coordenada] -> Coordenada -> [Coordenada]
movimientosValidos bosque visitadas (x, y) =
  filter (\c -> dentroLimites bosque c && notElem c visitadas)
    [ (x+1, y)       -- abajo
    , (x, y+1)       -- derecha
    , (x+1, y+1)     -- diagonal abajo-derecha
    , (x-1, y)       -- arriba (solo si no visitada)
    , (x, y-1)       -- izquierda (solo si no visitada)
    ]

-- Calcula la nueva energía con penalizaciones
energiaNueva :: Int -> Int -> Coordenada -> Coordenada -> Int
energiaNueva actual valor origen destino =
  let penalizacionDiagonal = if destino == (fst origen + 1, snd origen + 1) then 2 else 0
      penalizacionTrampa = if valor == 0 then 3 else 0
  in actual + valor - penalizacionDiagonal - penalizacionTrampa

-- Busca el mejor camino desde posición y energía dadas (con memo)
buscarMejorCaminoDesde :: Bosque -> Mago -> Memo -> (Maybe [(Coordenada, Int)], Memo)
buscarMejorCaminoDesde bosque (Mago pos e visitadas) memo
  | e < 0 = (Nothing, memo)
  | pos == destino = (Just [(pos, e)], Map.insert (pos, e) (Just [(pos, e)]) memo)
  | otherwise =
      case Map.lookup (pos, e) memo of
        Just res -> (res, memo)
        Nothing ->
          let coordsVisitadas = pos : visitadas
              vecinos = movimientosValidos bosque coordsVisitadas pos
              (mejorCamino, memoFinal) = foldl
                (\(mejor, m) siguiente ->
                   let v = valorCelda bosque siguiente
                       eNueva = energiaNueva e v pos siguiente
                       magoNuevo = Mago siguiente eNueva coordsVisitadas
                       (resCam, m2) = buscarMejorCaminoDesde bosque magoNuevo m
                   in case resCam of
                        Nothing -> (mejor, m2)
                        Just camino ->
                          if mejor == Nothing || energiaTotal camino > energiaTotal (unwrap mejor)
                            then (Just ((pos, e) : camino), m2)
                            else (mejor, m2)
                ) (Nothing, memo) vecinos
              memoNuevo = Map.insert (pos, e) mejorCamino memoFinal
          in (mejorCamino, memoNuevo)
  where
    destino = (length bosque - 1, length (head bosque) - 1)
    energiaTotal = sum . map snd
    unwrap (Just x) = x
    unwrap Nothing = error "unwrap Nothing"

-- Función pública para obtener el mejor camino desde el inicio
buscarMejorCamino :: Bosque -> Mago -> [(Coordenada, Int)]
buscarMejorCamino bosque mago =
  case buscarMejorCaminoDesde bosque mago Map.empty of
    (Just camino, _) -> camino
    (Nothing, _) -> []

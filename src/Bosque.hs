module Bosque (Bosque, buscarMejorCamino) where

import Mago
import qualified Data.Map.Strict as Map
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)

type Bosque = [[Int]]
type Energia = Int
type Camino = [(Coordenada, Energia)]
type Memo = Map.Map (Coordenada, Energia) (Maybe Camino)

-- Dimensiones del bosque
alto :: Bosque -> Int
alto = length

ancho :: Bosque -> Int
ancho bosque = if null bosque then 0 else length (head bosque)

-- Verifica si la coordenada está dentro del bosque
dentroLimites :: Bosque -> Coordenada -> Bool
dentroLimites bosque (x,y) =
  x >= 0 && y >= 0 && x < alto bosque && y < ancho bosque

-- Valor de la celda en la coordenada
valorCelda :: Bosque -> Coordenada -> Int
valorCelda bosque (x,y) = (bosque !! x) !! y

-- Movimientos permitidos según las reglas del enunciado
movimientos :: Bosque -> [Coordenada] -> Coordenada -> [Coordenada]
movimientos bosque visitados (x,y) = filter dentro (derechaAbajo ++ condicionales)
  where
    -- Siempre permitidos (si están dentro del bosque)
    derechaAbajo = [(x+1,y), (x,y+1), (x+1,y+1)]
    
    -- Solo si no se ha visitado antes
    condicionales = filter (`notElem` visitados) [(x-1,y), (x,y-1)]
    
    dentro c = dentroLimites bosque c

-- Calcula nueva energía con penalizaciones
energiaNueva :: Energia -> Int -> Coordenada -> Coordenada -> Energia
energiaNueva e valor (xO,yO) (xD,yD) =
  let penalDiag = if (xD == xO + 1) && (yD == yO + 1) then 2 else 0
      penalTrampa = if valor == 0 then 3 else 0
  in e + valor - penalDiag - penalTrampa

-- Busca el mejor camino usando memoización
buscarMejorCaminoMemo :: Bosque -> Mago -> Memo -> (Maybe Camino, Memo)
buscarMejorCaminoMemo bosque mago memo
  | energia mago < 0 = (Nothing, memo)
  | pos mago == destino = (Just [(pos mago, energia mago)], memo)
  | otherwise =
      case Map.lookup (pos mago, energia mago) memo of
        Just res -> (res, memo)
        Nothing ->
          let sigs = movimientos bosque (visitados mago) (pos mago)
              (mejoresCaminos, memo') = foldl
                (\(acc, m) c ->
                   let val = valorCelda bosque c
                       eNueva = energiaNueva (energia mago) val (pos mago) c
                       magoNuevo = Mago c eNueva (pos mago : visitados mago)
                       (resCam, m2) = buscarMejorCaminoMemo bosque magoNuevo m
                   in case resCam of
                        Just camino -> (camino : acc, m2)
                        Nothing -> (acc, m2)
                )
                ([], memo) sigs
          in if null mejoresCaminos
             then (Nothing, Map.insert (pos mago, energia mago) Nothing memo')
             else
               let mejor = maximumBy (comparing ultimaEnergia) mejoresCaminos
                   caminoCompleto = (pos mago, energia mago) : mejor
                   memoFinal = Map.insert (pos mago, energia mago) (Just caminoCompleto) memo'
               in (Just caminoCompleto, memoFinal)
  where
    destino = (alto bosque - 1, ancho bosque - 1)
    ultimaEnergia camino = snd (last camino)

-- Función pública para llamar desde Main
buscarMejorCamino :: Bosque -> Energia -> Maybe Camino
buscarMejorCamino bosque energiaInicial =
  let magoInicial = Mago (0,0) (energiaInicial + valorCelda bosque (0,0)) []
      (res, _) = buscarMejorCaminoMemo bosque magoInicial Map.empty
  in res

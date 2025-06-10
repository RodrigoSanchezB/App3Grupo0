import Mago (Mago(..))
import Bosque (Bosque, buscarMejorCamino)

main :: IO ()
main = do
  let bosque :: Bosque
      bosque = [ [ 2, -3, 1, 0, 2, 3]
               , [-5, 4, -2, 1, 0, -4]
               , [ 1, 3, 0, -3, 2, 2]
               , [ 2, -1, 4, 0, -5, 1]
               , [ 0, 2, -3, 3, 4, -1]
               , [ 1, 0, 2, -2, 1, 5]]

      energiaInicial = 12
      inicio = (0, 0)
      magoInicial = Mago inicio energiaInicial []

      mejorCamino = buscarMejorCamino bosque magoInicial

  putStrLn "Mejor camino encontrado (coordenadas con energía):"
  mapM_ (\(coord, e) -> putStrLn $ show coord ++ " con energía: " ++ show e) mejorCamino
  putStrLn $ "\nEnergía final: " ++
             (if null mejorCamino then "No hay camino válido"
              else show $ snd (last mejorCamino)
             )

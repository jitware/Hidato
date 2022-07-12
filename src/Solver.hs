module Solver (backtraking,solve ) where
import Matrix
import Lib 

-- For para el backtraking 
--- args:
---- f: Funcion a ejecutar parametros del for
---- matrix: Tablero
---- acc: Variable acumulada en el for lo que retorna la funcion f [True, matrix]
---- xs: La lista que se va a iterar
---- end: la tupla final
---- Value: valor del siguiente vecino
---- boxes: son las casillas marcadas
myfold_bool f matrix acc xs  end value boxes  = aux matrix acc xs  end value boxes
  where  aux matrix acc []  end value boxes     = acc
        --  aux matrix (True, _) _  end value boxes = acc
         aux matrix acc (x:xs)  end value boxes = aux matrix (f matrix acc x end value boxes) xs  end value boxes

-- f_back matrix acc neigbor end value boxes = 
--     if (fst acc) == True
--         then acc
--         else backtraking neigbor end boxes (setInMatrix matrix value (neigbor!!0) (neigbor!!1))

backtraking :: [Int] -> [Int] -> [[Int]] -> [[Int]] ->(Bool, [[Int]])
backtraking current end boxes matrix = 
    -- Comprueba que haya llegado al objetivo por el camino correcto 
    -- sino regresa hacia atrás y se mueve por otra casilla
    if index matrix current  == index matrix end
        then if current == end
                then 
                let t_last = lastIn boxes 
                    in if current == t_last
                        then (True, matrix)
                         else let t_next = nextBox current boxes
                                in backtraking end t_next boxes matrix
                                
                else (False, [])
        -- dame los vecinos
        else let neigbors = validNeigbors current matrix
                in myfold_bool f_back matrix (False, []) neigbors end ((index matrix current)+1) boxes
                where
                    f_back matrix acc neigbor end value boxes = if (fst acc) == True
                        then acc
                        else backtraking neigbor end boxes (setInMatrix matrix value (neigbor!!0) (neigbor!!1))

solve matrix = 
    let boxes = ord_boxes matrix (boxCoor matrix)
        current = posUno matrix
        end = boxes!!0
        in backtraking current end boxes matrix

-- let matrix =   [[0  ,33 ,35 ,0  ,0  ,-1 ,-1 ,-1 ], [0  ,0  ,24 ,22 ,0  ,-1 ,-1 ,-1 ], [0  ,0  ,0  ,21 ,0  ,0  ,-1 ,-1 ], [0  ,26 ,0  ,13 ,40 ,11 ,-1, -1 ], [27 ,0  ,0  ,0  ,9  ,0  ,1  ,-1 ], [-1 ,-1 ,0  ,0  ,18 ,0  ,0  ,-1 ], [-1 ,-1 ,-1 ,-1 ,0  ,7  ,0  ,0  ], [-1 ,-1 ,-1 ,-1 ,-1 ,-1 ,5  ,0  ]]
-- solve matrix
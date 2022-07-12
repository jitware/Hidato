module Solver (backtraking,solve ) where
import Matrix
import Lib 
-- For para el backtraking 
--- args:
---- f: Funcion a ejecutar parametros del for
---- matrix: Tablero
---- ys: Variable acumulada en el for lo que retorna la funcion f [True, matrix]
---- xs: La lista que se va a iterar
---- end: la tupla final
---- Value: valor del siguiente vecino
---- boxes: son las casillas marcadas
myfold_bool f matrix ys xs  end value boxes  = aux matrix ys xs  end value boxes
  where  aux matrix ys []  end value boxes     = /ys
        --  aux matrix (True, _) _  end value boxes = ys
         aux matrix ys (x:xs)  end value boxes = aux matrix (f matrix ys x end value boxes) xs  end value boxes


f_back matrix acc neigbor end value boxes = 
    if (fst acc) == True
        then acc
        else backtraking neigbor end boxes (setInMatrix matrix value (neigbor!!0) (neigbor!!1))

backtraking :: [Int] -> [Int] -> [[Int]] -> [[Int]] ->(Bool, [[Int]])
backtraking current end boxes matrix = 
    -- Valor de table[current_x][current_y]==table[end_x][end_y] en ese caso
    -- Comprueba que tengan la misma coordena sino return false
    -- En caso de tenerla si hay mas casilla actualiza current y end y entra otra vez
    -- Sino es la solucion 
    -- current = [x,y]  end = [t,z]
    if index matrix current  == index matrix end
        then if current == end
                then 
                let t_last = last_box boxes 
                    in if current == t_last
                        then (True, matrix)
                         else let t_next = next_box current boxes
                                in backtraking end t_next boxes matrix
        
                else (False, [])
        -- dame los vecinos
        else let neigbors = valid_neigbors current matrix
                in myfold_bool f_back matrix (False, []) neigbors end ((index matrix current)+1) boxes

solve matrix = 
    let boxes = ord_boxes matrix (boxCoor matrix)
        current = posUno matrix
        end = boxes!!0
        in backtraking current end boxes matrix


--let matrix = [[1,0,3,0],[6,0,0,0]]
--current = [0,0]
--end = [0,2]
--boxes = [[0,2],[1,0]]
--backtraking current end boxes matrix

-- let matrix =   [[0  ,33 ,35 ,0  ,0  ,-1 ,-1 ,-1 ], [0  ,0  ,24 ,22 ,0  ,-1 ,-1 ,-1 ], [0  ,0  ,0  ,21 ,0  ,0  ,-1 ,-1 ], [0  ,26 ,0  ,13 ,40 ,11 ,-1, -1 ], [27 ,0  ,0  ,0  ,9  ,0  ,1  ,-1 ], [-1 ,-1 ,0  ,0  ,18 ,0  ,0  ,-1 ], [-1 ,-1 ,-1 ,-1 ,0  ,7  ,0  ,0  ], [-1 ,-1 ,-1 ,-1 ,-1 ,-1 ,5  ,0  ]]
-- boxes = ord_boxes matrix (boxCoor matrix)
-- 
-- end = boxes!!0
-- del = backtraking current end boxes matrix
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

-- Agrega los valores unicos
changeMatrix matrix value neigbor = 
    (setInMatrix matrix value (neigbor!!0) (neigbor!!1))
    -- fullMatrixUniques ((setInMatrix matrix value (neigbor!!0) (neigbor!!1)))

updateEnd :: [[Int]]->Int->[Int]
updateEnd matrix n = 
    let boxes = ord_boxes matrix (boxCoor matrix n)
    in if boxes == [] 
        then []
        else boxes!!0

backtraking :: [Int] -> [Int] -> [[Int]] -> [[Int]] ->(Bool, [[Int]])
backtraking current end matrix = 
    -- Comprueba que haya llegado al objetivo por el camino correcto 
    -- sino regresa hacia atrás y se mueve por otra casilla
    if index matrix current  == index matrix end
        then if current == end
                then    let next_end = updateEnd matrix ((index matrix end)+1)
                        in if next_end == []
                            then (True, matrix)
                            else backtraking end next_end matrix                
                else (False, [])
        else
            ---si mi sucesor es el end buscalo en sus vecinos
            let neigbors = validNeigbors current matrix
                in myfold_bool fun matrix (False, []) neigbors end ((index matrix current)+1)
                where
                    fun matrix acc neigbor end value  = if (fst acc) == True
                        then acc
                        else let newMatrix = (changeMatrix matrix value neigbor)
                             -- new end = updateEnd newMatrix value
                             in backtraking neigbor end newMatrix

solve matrix = 
    let current = posUno matrix
        end = updateEnd matrix 2 
        in backtraking current end matrix

-- let matrix =   [[0  ,33 ,35 ,0  ,0  ,-1 ,-1 ,-1 ], [0  ,0  ,24 ,22 ,0  ,-1 ,-1 ,-1 ], [0  ,0  ,0  ,21 ,0  ,0  ,-1 ,-1 ], [0  ,26 ,0  ,13 ,40 ,11 ,-1, -1 ], [27 ,0  ,0  ,0  ,9  ,0  ,1  ,-1 ], [-1 ,-1 ,0  ,0  ,18 ,0  ,0  ,-1 ], [-1 ,-1 ,-1 ,-1 ,0  ,7  ,0  ,0  ], [-1 ,-1 ,-1 ,-1 ,-1 ,-1 ,5  ,0  ]]
-- solve matrix
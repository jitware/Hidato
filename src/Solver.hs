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

tLoop f matrix acc xs  end value     = aux matrix acc xs  end value 
  where  aux matrix acc []  end value      = acc
         aux matrix acc (x:xs)  end value  = if (fst acc) == True
                then acc
                else aux matrix (f matrix acc x end value ) xs  end value 

-- Agrega los valores unicos
changeMatrix matrix value neigbor = setInMatrix matrix value (neigbor!!0) (neigbor!!1)

-- changeMatrix :: [[Int]]->Int->[Int]->[[Int]]
-- changeMatrix matrix value neigbor =  
--     fullMatrixUniques ((setInMatrix matrix value (neigbor!!0) (neigbor!!1)))

updateEnd :: [[Int]]->Int->[Int]
updateEnd matrix n = 
    let boxes = ord_boxes matrix (boxCoor matrix n)
    in if boxes == [] 
        then []
        else boxes!!0

backtraking :: [Int] -> [Int] -> [[Int]] ->(Bool, [[Int]])
backtraking current end matrix = 
    -- Comprueba que haya llegado al objetivo por el camino correcto 
    -- sino regresa hacia atrás y se mueve por otra casilla
    
    if index matrix current  == index matrix end
        then if current == end
                then  let next_end = updateEnd matrix ((index matrix end)+1)
                        in if next_end == []
                            then (True, matrix)
                            -- else  (True, matrix)
                            else backtraking end next_end matrix                
                else (False, [])
        else let neigbors = validNeigbors current matrix
            in tLoop fun matrix (False, []) neigbors end ((index matrix current)+1)
                where
                    fun matrix acc neigbor end value  =
                            let newMatrix = changeMatrix matrix value neigbor 
                                -- newEnd = updateEnd newMatrix value
                                newEnd = end
                            in 
                                if newEnd /= []
                                    then backtraking neigbor newEnd newMatrix
                                    else (True,matrix)

solve matrix = 
    let current = posUno matrix
        newMatrix = fullMatrixUniques matrix
        end = updateEnd newMatrix 1
    in backtraking current end newMatrix

--- let matrix = [[0, 0  , 0,  0, 0,  0,  0 ],[0, 0  , 1,  0, 0,  0, 0 ],[0, 0 , 0,  0, 0,  0, 0 ],[0, 0 , 0,  0, 0,  0, 0 ],[0, 0 , 0,  0, 40, 0,  0 ],[0, 0  , 0,  0, 0,  0,  0 ],[0, 0  , 0,0,0,  0,  0 ]]
--- let matrix = [[1,0,-1,6],[0,-1,0,0]]
---  let matrix = [[1,2,-1,6],[2,-1,0,0]]
-- Caso 1
-- let matrix = [[1,2,-1,6],[3,-1,0,0]]
-- let matrix =   [[0  ,33 ,35 ,0  ,0  ,-1 ,-1 ,-1 ], [0  ,0  ,24 ,22 ,0  ,-1 ,-1 ,-1 ], [0  ,0  ,0  ,21 ,0  ,0  ,-1 ,-1 ], [0  ,26 ,0  ,13 ,40 ,11 ,-1, -1 ], [27 ,0  ,0  ,0  ,9  ,0  ,1  ,-1 ], [-1 ,-1 ,0  ,0  ,18 ,0  ,0  ,-1 ], [-1 ,-1 ,-1 ,-1 ,0  ,7  ,0  ,0  ], [-1 ,-1 ,-1 ,-1 ,-1 ,-1 ,5  ,0  ]]
-- solve matrix
-- changeMatrix matrix 1 [0,1]
-- let new = fullMatrixUniques matrix 
module Solver (solveHidato,solve ) where
import Matrix
import Lib 

-- For para el solveHidato 
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

solveHidato :: [Int] -> [Int] -> [[Int]] ->(Bool, [[Int]])
solveHidato current end Hidato = 
    -- Comprueba que haya llegado al objetivo por el camino correcto 
    -- sino regresa hacia atrás y se mueve por otra vecino de la casilla
    if index Hidato current  == index Hidato end
        then if current == end
                then  
                    let next_end = updateEnd Hidato ((index Hidato end)+1)
                    in if next_end == []
                        then (True, Hidato)
                        else solveHidato end next_end Hidato                

                else (False, [])
        else let neigbors = validNeigbors current Hidato
             in tLoop fun Hidato (False, []) neigbors end ((index Hidato current)+1)
                where
                    fun Hidato acc neigbor end value  =
                            let newHidato = changeMatrix Hidato value neigbor 
                            in  solveHidato neigbor end newHidato 
                            
solve Hidato = 
    let current = posUno Hidato
        newHidato = fullMatrixUniques Hidato
        end = updateEnd newHidato 1
    in solveHidato current end newHidato

--- let matrix = [[1,0,-1,6],[0,-1,0,0]]
---  let matrix = [[1,2,-1,6],[2,-1,0,0]]
-- Caso 1
-- let matrix = [[1,2,-1,6],[3,-1,0,0]]
-- let matrix =   [[0  ,33 ,35 ,0  ,0  ,-1 ,-1 ,-1 ], [0  ,0  ,24 ,22 ,0  ,-1 ,-1 ,-1 ], [0  ,0  ,0  ,21 ,0  ,0  ,-1 ,-1 ], [0  ,26 ,0  ,13 ,40 ,11 ,-1, -1 ], [27 ,0  ,0  ,0  ,9  ,0  ,1  ,-1 ], [-1 ,-1 ,0  ,0  ,18 ,0  ,0  ,-1 ], [-1 ,-1 ,-1 ,-1 ,0  ,7  ,0  ,0  ], [-1 ,-1 ,-1 ,-1 ,-1 ,-1 ,5  ,0  ]]
-- solve matrix
-- changeMatrix matrix 1 [0,1]
-- let new = fullMatrixUniques matrix 
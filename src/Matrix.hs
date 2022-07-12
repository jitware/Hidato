module Matrix (nextBox,index,ord_boxes, setInMatrix,getIndex , validNeigbors, lastIn, boxCoor ) where
import Lib

index matrix cord = matrix!!(cord!!0)!!(cord!!1)

myFoldl f matrix ys xs current = aux matrix ys xs current
  where  aux matrix ys [] current     = ys
         aux matrix ys (x:xs) current = aux matrix (f matrix ys x current) xs current

isValidCord  x y matrix = (x < length matrix) && (y <  length (matrix!!0)) && (x>=0) && (y>=0)

--Filtro para vecinos
neigborValue x y n matrix = (matrix!!(x)!!(y)==0) || (matrix!!(x)!!(y)==n)

-- Suma 2 coordenadas
sumCord cord cord1 = [cord!!0 + cord1!!0 , cord!!1 + cord1!!1]

--Validar una coordenada segun la tabla
---args:
--- current: [[x,y]] donde x e y son las coordenadas
--- n: El valor posible de la siguiente casilla
--- matrix: Tablero
validCord current n matrix = 
    let x = current!!0!!0 ; y = current!!0!!1 ; 
        in  if (isValidCord x y matrix) && (neigborValue x y n matrix)
            then current
            else []

--  Chequea cuales de sus vecinos son validos
--   | 0 | 0 | 0 |
--   | 0 | x | 0 |
--   | 0 | 0 | 0 |
validNeigbors current matrix=
    myFoldl func matrix []  [[1,0],[1,1],[1,-1],[0,1],[0,-1],[-1,0],[-1,1],[-1,-1]] current
    where
        func matrix x num current = x ++ validCord [sumCord num current] ((index matrix current)+1) matrix

--devuele el último de la lista
lastIn boxes = boxes!!((length boxes)-1)

-- Busca las coordenas de un valor
getIndex matrix n = getIndex_1 matrix 0 n
getIndex_1 matrix x n = 
    if x < length(matrix)
        then
            if (matrix!!x) == n
                then x
                else getIndex_1 matrix (x+1) n
        else -1        
            
nextBox ys xs = xs!!((getIndex xs ys)+1)

---Ordenar
quicksort :: [[Int]]-> [[Int]]-> [[Int]]
quicksort matrix [] = []
quicksort matrix (x:xs) =
    let smallerSorted = quicksort matrix [a | a <- xs, (index matrix a) <= (index matrix x)]
        biggerSorted  = quicksort matrix [a | a <- xs, (index matrix a) > (index matrix x)]
    in  smallerSorted ++ [x] ++ biggerSorted

ord_boxes matrix xs = quicksort matrix xs
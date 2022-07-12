module Matrix (next_box,index,ord_boxes, setInMatrix,get_index , valid_neigbors, last_box, boxCoor ) where
import Lib

index matrix cord = matrix!!(cord!!0)!!(cord!!1)

myfold f matrix ys xs current = aux matrix ys xs current
  where  aux matrix ys [] current     = ys
         aux matrix ys (x:xs) current = aux matrix (f matrix ys x current) xs current

is_valid_cord  x y matrix = (x < length matrix) && (y <  length (matrix!!0)) && (x>=0) && (y>=0)

--Filtro para vecinos
neigbor_value x y n matrix = (matrix!!(x)!!(y)==0) || (matrix!!(x)!!(y)==n)

--Validar una coordenada segun la tabla
---args:
--- current: [[x,y]] donde x e y son las coordenadas
--- n: El valor posible de la siguiente casilla
--- matrix: Tablero
valid_cord current n matrix = 
    let x = current!!0!!0 ; y = current!!0!!1 ; 
        in  if (is_valid_cord x y matrix) && (neigbor_value x y n matrix)
            then current
            else []

-- Suma 2 coordenadas
sum_cord cord cord1 = [cord!!0 + cord1!!0 , cord!!1 + cord1!!1]

func matrix x num current = x ++ valid_cord [[num!!0 + current!!0, num!!1 + current!!1]] ((index matrix current)+1) matrix
lopp matrix cord lista current = myfold func matrix cord lista current 

--
valid_neigbors current matrix=
    lopp matrix [] [[1,0],[1,1],[1,-1],[0,1],[0,-1],[-1,0],[-1,1],[-1,-1]] current

last_box boxes = boxes!!((length boxes)-1)
--- a = [[0,0,0],[0,1,0],[2,0,0]]
--- current = [1,1]

-- Busca las coordenas de un valor
get_index matrix n = get_index_1 matrix 0 n
get_index_1 matrix x n = 
    if x < length(matrix)
        then
            if (matrix!!x) == n
                then x
                else get_index_1 matrix (x+1) n
        else -1        
            
next_box ys xs = xs!!((get_index xs ys)+1)

---------------------------------------------------------------------------------------
---
quicksort :: [[Int]]-> [[Int]]-> [[Int]]
quicksort matrix [] = []
quicksort matrix (x:xs) =
    let smallerSorted = quicksort matrix [a | a <- xs, (index matrix a) <= (index matrix x)]
        biggerSorted  = quicksort matrix [a | a <- xs, (index matrix a) > (index matrix x)]
    in  smallerSorted ++ [x] ++ biggerSorted

ord_boxes matrix xs = quicksort matrix xs



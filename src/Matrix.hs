module Matrix (next_box,index, setInMatrix,get_index , valid_neigbors, last_box, boxCoor ) where

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
setInList list x pos = (take pos list)++[x]++(drop(pos+1)list)

-- setInMatrix agrega el elemento `x` a la posicion <posx.posy> de la matriz `matrix`
setInMatrix matrix x posx posy = setInList matrix (setInList ( matrix!!posx) x posy) posx 

quicksort :: [[Int]]-> [[Int]]-> [[Int]]
quicksort matrix [] = []
quicksort matrix (x:xs) =
    let smallerSorted = quicksort matrix [a | a <- xs, (index matrix a) <= (index matrix x)]
        biggerSorted  = quicksort matrix [a | a <- xs, (index matrix a) > (index matrix x)]
    in  smallerSorted ++ [x] ++ biggerSorted

--Dado una matriz devuelve la lista de coordenadas de los valores distintos de cero, 1 y -1
boxCoor matrix = boxCoor_1 matrix 0 (remove(remove(remove(remove (allValuesMatrix matrix) (-2)) 0)(maxMatrix matrix+1))1) []
boxCoor_1 matrix i all result=
    if i < len(all)
        then
            boxCoor_1 matrix (i+1) all (add result (posValue (all!!i) matrix))
        else 
            result
               
--Dado un valir devuelve su posicion en la matriz
posValue value matrix = posValue_1 value matrix 0 0
posValue_1 value matrix x y = 
    if y < len(index matrix x)
        then
            if (indexMatriz matrix x y) == value
                then [x,y]
                else posValue_1 value matrix x (y+1)
        else        
            if x < (len(matrix)-1)
                then posValue_1 value matrix (x+1) 0
                else [-1]

maxMatrix matrix = maxMatrix_1 matrix [] 0
maxMatrix_1 matrix result y= 
    if y < (len matrix)
        then
            maxMatrix_1 matrix (add result (maximum (index matrix y))) (y+1)
        else
            maximum result

-- Dada una matriz devuelve una lista con todos los valores que ya contiene, ademas se le agrega -1, 0 y 1
allValuesMatrix :: [[Int]]-> [Int]
allValuesMatrix matrix = remove ( allValuesMatrix_1 matrix [0,-2] 0 0 ) (-1)
allValuesMatrix_1 matrix result x y =
    if y < len(index matrix x)
        then
           allValuesMatrix_1 matrix (addR result (indexMatriz matrix x y)) x (y+1)
        else
            if x < (len (matrix)-1)   
                then
                    allValuesMatrix_1 matrix result (x+1) 0
                else
                    add result ((maximum result) +1)


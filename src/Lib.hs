module Lib (boxCoor,posValue, posUno, maxMatrix, allValuesMatrix,setInMatrix, equalOrBest,roundMatrixUniques,len
,indexMatriz,fullMatrixUniques, mov, zoom, desZoom, add ) where

--Movimientos alrededor de una casilla
mov 0 = [-1,-1]
mov 1 = [-1,0]
mov 2 = [-1,1]
mov 3 = [0,-1]
mov 4 = [0,1]
mov 5 = [1,-1]
mov 6 = [1,0]
mov 7 = [1,1]

index list x = list!!x -- devuelve el elemento de la lista que se encuentra en la posicion x

indexMatriz :: [[Int]]-> Int -> Int -> Int
indexMatriz matriz x y = matriz!!x!!y  -- devuelve el elemento de la matriz que se encuentra en la posicion <x,y>

len list = length list -- devuelve el length de la lista

-- setInList agrega el elemento `x` a la posicion `pos` de la lista `list`
setInList list x pos = (take pos list)++[x]++(drop(pos+1)list) 

-- setInMatrix agrega el elemento `x` a la posicion <posx.posy> de la matriz `matrix`
setInMatrix matrix x posx posy = setInList matrix (setInList (index matrix posx) x posy) posx 

add list x = list++[x] -- agrega el elemento x a la lista 
addR list x = if (contain list x) == False then add list x else list -- agrega si no pertenece

removeFromPos list pos =(take pos list)++(drop(pos+1)list) -- elimina el elemento que se encuentra en la posicion `pos` de la lista
--remove Elimina el elemento x de la lista
remove list x = if (contain list x) == True then remove1 list x 0 else list
remove1 list x pos = if pos == ((len list)-1) 
    then (if (index list pos) == x then removeFromPos list pos else list) 
    else (if (index list pos) == x then removeFromPos list pos else remove1 list x (pos+1))

contain list x = x `elem` list -- devuelve en un booleano si la lista contiene el elemento x

--dado una lista l1 y una lista l2 elimina de l1 todos los elementos de l2
nand :: [Int] -> [Int] -> [Int]
nand l1 l2 = nand1 l1 l2 0
nand1 l1 l2 pos = if (pos+1) == (len l2) 
    then remove l1 (index l2 pos) 
    else nand1 (remove l1 (index l2 pos)) l2 (pos+1)

--zoom Dado una matriz la rodea de un borde con todos los valores `-1` 
zoom :: [[Int]] -> [[Int]]
zoom matrix = zoom1 matrix [[]] 0 0 0 0 
zoom1 :: [[Int]] -> [[Int]] -> Int-> Int-> Int-> Int ->[[Int]] 
zoom1 matrix matrix_result posX posY pos_X pos_Y = 
    if posY == (len(index matrix 0)+2) 
        then 
            if posX < (len(matrix)+1) 
                then
                    zoom1 matrix (add matrix_result (index matrix_result 0)) (posX+1) posY pos_X pos_Y 
                else 
                    if pos_Y < (len(index matrix 0))
                        then 
                            zoom1 matrix (setInMatrix matrix_result (indexMatriz matrix pos_X pos_Y) (pos_X+1) (pos_Y+1)) posX posY (pos_X) (pos_Y+1)
                        else
                            if pos_X < (len(matrix) -1)
                                then 
                                    zoom1 matrix matrix_result posX posY (pos_X+1) (0)
                                else 
                                    matrix_result
        else    
            zoom1 matrix (setInList matrix_result (-1:(index matrix_result 0)) 0) (posX) (posY+1) pos_X pos_Y

-- Elimina el zoom de una matriz
desZoom matrix = desZoom1 matrix False 0
desZoom1 matrix check x = 
    if check == False
        then
            desZoom1 (removeFromPos (removeFromPos matrix ((len matrix)-1)) 0) True 0
        else
            if x < len(matrix)
                then
                    desZoom1 (setInList matrix (remove(remove (index matrix x) (-1))(-1)) x) True (x+1)
                else
                    matrix    

--dado una posicion y una matriz devuelve todos los valores admisibles por esa posicion [caso general]
allValues :: [[Int]] -> Int -> Int -> [Int]
allValues matrix x y = allValues1 matrix x y (zoom matrix)

allValues1 :: [[Int]]-> Int-> Int ->[[Int]] -> [Int] 
-- z es una instancia del zoom de la matriz para no llamarla varias veces
allValues1 matrix x y z = 
            (addR
            (addR
            (addR
            (addR
            (addR
            (addR
            (addR
            (addR
            (addR 
            (addR
            (addR
            (addR
            (addR
            (addR
            (addR 
            (addR [] 
            ((indexMatriz (z) (x+2) (y+1))+1))
            ((indexMatriz (z) (x+2) (y+2))+1))
            ((indexMatriz (z) (x+2)  y   )+1))
            ((indexMatriz (z) (x)   (y+1))+1))
            ((indexMatriz (z) (x)   (y+2))+1))
            ((indexMatriz (z) (x)    y   )+1))
            ((indexMatriz (z) (x+1)  y   )+1))
            ((indexMatriz (z) (x+1) (y+2))+1))
            ((indexMatriz (z) (x+2) (y+1))-1))
            ((indexMatriz (z) (x+2) (y+2))-1))
            ((indexMatriz (z) (x+2)  y   )-1))
            ((indexMatriz (z) (x)   (y+1))-1))
            ((indexMatriz (z) (x)   (y+2))-1))
            ((indexMatriz (z) (x)    y   )-1))
            ((indexMatriz (z) (x+1)  y   )-1))
            ((indexMatriz (z) (x+1) (y+2))-1))

-- Dada una matriz devuelve una lista con todos los valores que ya contiene, ademas se le agrega -1, 0 y 1
allValuesMatrix :: [[Int]]-> [Int]
allValuesMatrix matrix = remove ( allValuesMatrix1 matrix [0,-2] 0 0 ) (-1)
allValuesMatrix1 matrix result x y =
    if y < len(index matrix x)
        then
           allValuesMatrix1 matrix (addR result (indexMatriz matrix x y)) x (y+1)
        else
            if x < (len (matrix)-1)   
                then
                    allValuesMatrix1 matrix result (x+1) 0
                else
                    add result ((maximum result) +1)

--Dado una matriz y una posicion devuelve los valores admisibles por esta posicion [comun]
admValues :: [[Int]] -> Int -> Int -> [Int]
admValues matrix x y = nand (allValues matrix x y) (allValuesMatrix matrix)

--Comprueba si la casilla esta entre dos diagonales de tal forma que solo puede tener un valor unico
uniqueDiag matrix x y = uniqueDiag1 matrix x y (zoom matrix)
uniqueDiag1 :: [[Int]] ->Int ->Int ->[[Int]]->[Int]
uniqueDiag1 matrix x y zom =
    if abs((indexMatriz zom (x+1+1) (y-1+1)) - (indexMatriz zom (x-1+1) (y+1+1))) == 2
        then
            if (indexMatriz zom (x+1+1) (y-1+1)) == -1 || (indexMatriz zom (x-1+1) (y+1+1)) == -1
                then
                    admValues matrix x y    
                else    
                    if (indexMatriz zom (x+1+1) (y-1+1)) == -1 || (indexMatriz zom (x+1+1) (y-1+1)) == 0
                        || (indexMatriz zom (x-1+1) (y+1+1)) == -1 || (indexMatriz zom (x-1+1) (y+1+1)) == 0
                        then
                            admValues matrix x y    
                        else    
                            [(((indexMatriz zom (x+1+1) (y-1+1))+(indexMatriz zom (x-1+1) (y+1+1)))`div`2)]
        else
            if abs((indexMatriz zom (x-1+1) (y-1+1)) - (indexMatriz zom (x+1+1) (y+1+1))) == 2
                then
                    if (indexMatriz zom (x+1+1) (y-1+1)) == -1 || (indexMatriz zom (x-1+1) (y+1+1)) == -1
                        then
                            admValues matrix x y    
                        else 
                            [((indexMatriz zom (x-1+1) (y-1+1)) + (indexMatriz zom (x+1+1) (y+1+1))`div`2)]
                else
                    admValues matrix x y    

--Analiza todos los vecinos y devuelve el valor que debe tener en caso de ser unico
uniqueInMid ::[[Int]] ->Int ->Int->[Int]
uniqueInMid matrix x y = uniqueInMid1 (zoom ( matrix)) (x+1) (y+1) 0 0
uniqueInMid1 ::[[Int]] ->Int ->Int->Int->Int->[Int]
uniqueInMid1 zom x y i j =
    if i < 8
        then
            if (abs((indexMatriz zom (x+((mov i)!!0)) (y+((mov i)!!1))) - (indexMatriz zom (x+((mov j)!!0)) (y+((mov j)!!1))))) == 2
                then  
                    if (indexMatriz zom (x+((mov i)!!0)) (y+((mov i)!!1))) == (-1) 
                        || (indexMatriz zom (x+((mov j)!!0)) (y+((mov j)!!1))) == (-1)
                        then
                            uniqueInMid1 zom x y (i+1) j
                        else
                            if (communneigbordEmpty zom 
                                (x+((mov i)!!0)) (y+((mov i)!!1)) (x+((mov j)!!0)) (y+((mov j)!!1))) == 1
                                then
                                    if (contain (allValuesMatrix zom)
                                        (((indexMatriz zom (x+((mov i)!!0)) (y+((mov i)!!1))) + (indexMatriz zom (x+((mov j)!!0)) (y+((mov j)!!1)))) `div` 2)) == True
                                        then
                                            uniqueInMid1 zom x y (i+1) j
                                        else
                                            [((indexMatriz zom (x+((mov i)!!0)) (y+((mov i)!!1))) + (indexMatriz zom (x+((mov j)!!0)) (y+((mov j)!!1)))) `div` 2] 
                                else 
                                    uniqueInMid1 zom x y (i+1) j
                else
                    uniqueInMid1 zom x y (i+1) j
        else 
            if j < 7
                then
                    uniqueInMid1 zom x y (j+1) (j+1)
                else
                    --si tiene un solo vecino que sea cero no se puede decir que pueda ser unico
                    -- luego pasar pot todos los vecinos consigo mismo(serian sus vecinos) y prenguntar
                    -- la cantidad vacios
                    if (communneigbordEmpty zom x y x y) > 0
                        then
                            [-1, -1]
                        else    
                            admValues zom x y    

--Devuelve la cantidad vecinos comunes que estan vacios
communneigbordEmpty :: [[Int]]->Int->Int->Int->Int->Int
communneigbordEmpty zom x1 y1 x2 y2 = communneigbordEmpty1 zom x1 y1 x2 y2 0 0 (communeNeibordsZeroIsOne zom x1 y1 x2 y2)

communneigbordEmpty1 :: [[Int]]->Int->Int->Int->Int->Int->Int->[[Int]]->Int
communneigbordEmpty1 zom x1 y1 x2 y2 result i matrixZero=
    if i < (len(communneigbords x1 y1 x2 y2))
        then
            communneigbordEmpty1 
            zom x1 y1 x2 y2 
            (result + matrixZero!!((communneigbords x1 y1 x2 y2)!!i!!0)!!((communneigbords x1 y1 x2 y2)!!i!!1)) 
            (i+1)
            matrixZero
        else
            result    

--Dada una matriz devuelve una matriz de 0 y 1 donde 0 es 1 y el resto es 0
zeroIsOne matrix = zeroIsOne1 matrix 0 0
zeroIsOne1 matrix x y =
    if y < (len (matrix!!x))
        then
            if (indexMatriz matrix x y) == 0
                then
                    zeroIsOne1 (setInMatrix matrix 1 x y) x (y+1)
                else
                    zeroIsOne1 (setInMatrix matrix 0 x y) x (y+1)
        else 
            if x < ((len matrix)-1)
            then
                zeroIsOne1 matrix (x+1) 0
            else 
                matrix   

--solo aplica el cambio a los vecinos comunes de un par de posiciones
communeNeibordsZeroIsOne matrix x1 y1 x2 y2= communeNeibordsZeroIsOne1 matrix (communneigbords x1 y1 x2 y2) 0 
communeNeibordsZeroIsOne1 matrix neig i=
    if i < len(neig)
        then
            if (indexMatriz matrix (neig!!i!!0) (neig!!i!!1)) == 0
                    then
                        communeNeibordsZeroIsOne1 (setInMatrix matrix 1 (neig!!i!!0) (neig!!i!!1)) neig (i+1)
                    else
                        communeNeibordsZeroIsOne1 (setInMatrix matrix 0 (neig!!i!!0) (neig!!i!!1)) neig (i+1)
        else 
            matrix    

neigbords :: Int->Int ->[[Int]]
neigbords x y =
    [[(x-1),(y-1)],
    [(x-1),(0 +y)],
    [(x-1),(1 +y)],
    [(0+x),(y-1 )],
    [(0+x),(1 +y)],
    [(1+x),(y-1 )],
    [(1+x),(0 +y)],
    [(1+x),(1 +y)]]

--devuelve una lista con todos los vecinos comunes
communneigbords x y x1 y1 =  communneigbords1 x y x1 y1 [] 0
communneigbords1 x y x1 y1 result i = 
    if i < 8
        then
            if (contain (neigbords x y) ((neigbords x1 y1)!!i)) == True
                then
                    communneigbords1 x y x1 y1 (add result ((neigbords x1 y1)!!i)) (i+1)
                else    
                    communneigbords1 x y x1 y1 result (i+1)
        else
            result            

endValues matrix x y = uniqueInMid matrix x y

--Dada una matriz devuelve el mayor valor de la misma 
maxMatrix matrix = maxMatrix1 matrix [] 0
maxMatrix1 matrix result y= 
    if y < (len matrix)
        then
            maxMatrix1 matrix (add result (maximum (index matrix y))) (y+1)
        else
            maximum result    

--Dado una posicion si solo tiene un adyacente accesible y su sucesor o antecesor no esta en el tablero entonces 
--lo asigna al accesible
adjUnique matrix x y = adjUnique1 matrix x y (adjEmptyPos matrix x y)
adjUnique1 matrix x y adj = 
    if (adjValues matrix x y) == 1
        then
            if contain (allValuesMatrix matrix) ((indexMatriz matrix x y)+1) == False
                then
                    if (indexMatriz matrix x y) /= (maxMatrix(matrix))
                        then
                            setInMatrix matrix ((indexMatriz matrix x y)+1) (adj!!0) (adj!!1)
                        else                
                            if (contain (allValuesMatrix matrix) ((indexMatriz matrix x y)-1)) == False
                                then
                                    if (indexMatriz matrix x y) /= 1
                                        then
                                            setInMatrix matrix ((indexMatriz matrix x y)-1) (adj!!0) (adj!!1)
                                        else
                                            matrix
                                else
                                    matrix            
                else                
                    if contain (allValuesMatrix matrix) ((indexMatriz matrix x y)-1) == False
                        then
                            if (indexMatriz matrix x y) /= 1
                                then
                                    setInMatrix matrix ((indexMatriz matrix x y)-1) (adj!!0) (adj!!1)
                                else
                                    matrix
                        else 
                            matrix            
        else 
            matrix                                    
    
--Dado una posicion y la matriz devuelve cuantos adyecentes accesibles(modificables) tiene
adjValues matrix x y = adjValues1 (zoom matrix) (x+1) (y+1) 0 0 (communeNeibordsZeroIsOne (zoom matrix) (x+1) (y+1) (x+1) (y+1))
--se le entra la matriz donde solo cambian de cero a 1 y de `k` a `0`  los valores vecinos de la casilla (los comunes consigo misma)
adjValues1 :: [[Int]]-> Int -> Int -> Int-> Int ->[[Int]] -> Int
adjValues1 matrix x y i result matrixZero=
    if i < 8
        then
            adjValues1 matrix x y (i+1) ((indexMatriz matrixZero (x + ((mov i)!!0)) (y + ((mov i)!!1)))+result) matrixZero
        else 
            result    

--Dado una posicion devuelve una posicion adyacente vacia
adjEmptyPos :: [[Int]]-> Int -> Int ->[Int]
adjEmptyPos matrix x y = adjEmptyPos1 (zoom matrix) (x+1) (y+1) 0
adjEmptyPos1 matrix x y i =
    if i < 8
        then
            if (indexMatriz (matrix) (x + ((mov i)!!0)) (y + ((mov i)!!1))) == 0
                then
                    [(x + ((mov i)!!0) -1),(y + ((mov i)!!1)-1)]
                else
                    adjEmptyPos1 matrix x y (i+1)
        else
            [-1,-1]

--Devuelve se la matriz resultante poseen al menos todos los valores fijos de la anterior
equalOrBest matrix matrixresult = equalOrBest1 matrix matrixresult 0 0
equalOrBest1 matrix matrixresult x y =
    if y < len(matrix!!x)
        then
            if ((indexMatriz matrix x y) /= 0) && ((indexMatriz matrix x y) /= -1)
                then
                    (indexMatriz matrix x y) == (indexMatriz matrixresult x y) &&
                    equalOrBest1 matrix matrixresult x (y+1)
                else
                    equalOrBest1 matrix matrixresult x (y+1)    
        else
            if x<(len(matrix)-1)
                then
                    equalOrBest1 matrix matrixresult (x+1) 0
                else
                    True        

-- llena la matriz de valores unicos, valores que siempre seran obligatorios
fullMatrixUniques matrix = fullMatrixUniques1 matrix True
fullMatrixUniques1 matrix check =
    if check == True
        then
            fullMatrixUniques2 matrix 0 0 0
        else
            matrix    
fullMatrixUniques2 matrix x y changes=
    if y < len(matrix!!x)
        then
            if (indexMatriz matrix x y) /= 0
                then
                    if (matrix) /= (adjUnique matrix x y) --si habra cambio hacer el cambio
                        then
                            fullMatrixUniques2 (adjUnique matrix x y) x (y+1) (changes+1)
                        else    
                            fullMatrixUniques2 matrix x (y+1) changes
                else
                    if(len(endValues matrix x y))==1 && (indexMatriz matrix x y) == 0 && ((endValues matrix x y)!!0) /= (-1)
                        then
                            fullMatrixUniques2 (setInMatrix matrix ((endValues matrix x y)!!0) x y) x (y+1) (changes+1)
                        else    
                            fullMatrixUniques2 matrix x (y+1) changes
        else
            if x < (len(matrix)-1)
                then
                    fullMatrixUniques2 matrix (x+1) 0 changes
                else 
                    fullMatrixUniques1 matrix (changes > 0)   

--Aplica valores unicos una sola vez en toda la matriz
roundMatrixUniques matrix = roundMatrixUniques1 matrix 0 0
roundMatrixUniques1 matrix x y =
    if y < len(matrix!!x)
        then
            if (indexMatriz matrix x y) /= 0
                then
                    if (matrix) /= (adjUnique matrix x y) --si habra cambio hacer el cambio
                        then
                            roundMatrixUniques1 (adjUnique matrix x y) x (y+1)
                        else    
                            roundMatrixUniques1 matrix x (y+1) 
                else
                    if(len(endValues matrix x y))==1 && (indexMatriz matrix x y) == 0 && ((endValues matrix x y)!!0) /= (-1)
                        then
                            roundMatrixUniques1 (setInMatrix matrix ((endValues matrix x y)!!0) x y) x (y+1)
                        else    
                            roundMatrixUniques1 matrix x (y+1) 
        else
            if x < (len(matrix)-1)
                then
                    roundMatrixUniques1 matrix (x+1) 0 
                else 
                    matrix 



--Dado una matriz devuelve la lista de coordenadas de los valores distintos de cero, 1 y -1
boxCoor matrix n  = boxCoor1 matrix 0 (filter (>n) (remove (allValuesMatrix matrix) (maxMatrix matrix+1))) []
boxCoor1 matrix i all result=
    if i < len(all)
        then
            boxCoor1 matrix (i+1) all (add result (posValue (all!!i) matrix))
        else 
            result

--Devuelve la posicion donde se encuentra el valor uno en la matriz
posUno matrix = posValue 1 matrix

--Dado un valir devuelve su posicion en la matriz
posValue value matrix = posValue1 value matrix 0 0
posValue1 value matrix x y = 
    if y < len(index matrix x)
        then
            if (indexMatriz matrix x y) == value
                then [x,y]
                else posValue1 value matrix x (y+1)
        else        
            if x < (len(matrix)-1)
                then posValue1 value matrix (x+1) 0
                else [-1]

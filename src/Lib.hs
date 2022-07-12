{-# LANGUAGE FlexibleContexts #-}
module Lib
    ( someFunc
    ) where

someFunc = putStrLn "someFunc"

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
                            if (communVecineEmpty zom 
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
                    uniqueInMid1 zom x y 0 (j+1)
                else
                    admValues zom x y    

--Devuelve la cantidad vecinos comunes que estan vacios
communVecineEmpty :: [[Int]]->Int->Int->Int->Int->Int
communVecineEmpty zom x1 y1 x2 y2 = communVecineEmpty1 zom x1 y1 x2 y2 0 0

communVecineEmpty1 :: [[Int]]->Int->Int->Int->Int->Int->Int->Int
communVecineEmpty1 zom x1 y1 x2 y2 result i =
    if i < (len(communVecines x1 y1 x2 y2))
        then
            communVecineEmpty1 
            zom x1 y1 x2 y2 
            (result + (zeroIsOne zom)!!((communVecines x1 y1 x2 y2)!!i!!0)!!((communVecines x1 y1 x2 y2)!!i!!1)) 
            (i+1)
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

vecines :: Int->Int ->[[Int]]
vecines x y =
    [[(x-1),(y-1)],
    [(x-1),(0 +y)],
    [(x-1),(1 +y)],
    [(0+x),(y-1 )],
    [(0+x),(1 +y)],
    [(1+x),(y-1 )],
    [(1+x),(0 +y)],
    [(1+x),(1 +y)]]

--devuelve una lista con todos los vecinos comunes
communVecines x y x1 y1 =  communVecines1 x y x1 y1 [] 0
communVecines1 x y x1 y1 result i = 
    if i < 8
        then
            if (contain (vecines x y) ((vecines x1 y1)!!i)) == True
                then
                    communVecines1 x y x1 y1 (add result ((vecines x1 y1)!!i)) (i+1)
                else    
                    communVecines1 x y x1 y1 result (i+1)
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

--Dada una matriz con numeros devuelve si es valida la misma, si es alcanzable 
validMatrix matrix = validMatrix1 matrix 0 0 
validMatrix1 matrix x y= 
    if y < len(index matrix x)
        then
            validPos matrix x y && validMatrix1 matrix x (y+1)
        else
            if x < ((len matrix) -1)
                then
                    validMatrix1 matrix (x+1) 0
                else
                    True
--dada una posicion analiza si es posible que el valor de la misma sea el que tiene
validPos matrix x y = 
    if (indexMatriz matrix x y) == -1 ||(indexMatriz matrix x y) == 0
        then
            True
        else    
            validPos1 matrix [] x y x y (zoom matrix)
validPos1 matrix adj x y countX countY zom = 
    if countY < y + 3
        then
            if countX == (x+1) && countY == (y+1)
                then
                    validPos1 matrix adj x y (countX) (countY+1) zom
                else 
                    validPos1 matrix (add adj (indexMatriz zom countX countY)) x y (countX) (countY+1) zom
        else 
            if countX < x+2
                then
                    validPos1 matrix adj x y (countX+1) 0 zom
                else
                    if (indexMatriz matrix x y) == (maxMatrix matrix) 
                        then
                            (contain adj ((maxMatrix matrix)-1)) || (contain adj 0)
                        else 
                            if (indexMatriz matrix x y) == 1 
                                then
                                    (contain adj 2) || (contain adj 0)
                                else
                                    ((contain adj ((indexMatriz matrix x y)-1)) && (contain adj ((indexMatriz matrix x y)+1))) 
                                    || 
                                    ((contain adj ((indexMatriz matrix x y)-1)) && (contain adj 0))
                                    ||
                                    ((contain adj ((indexMatriz matrix x y)+1)) && (contain adj 0))
                                    ||
                                    ((contain adj 0) && (contain (remove adj 0) 0))

--Dada una matriz completamente llena de numeros devuelve si la misma es solucion
validMatrixComplete matrix = validMatrixComplete1 matrix 0 0
validMatrixComplete1 matrix x y =
    if y < len(index matrix x)
        then
            validPosComplete matrix x y && validMatrixComplete1 matrix x (y+1)
        else
            if x < ((len matrix) -1)
                then
                    validMatrixComplete1 matrix (x+1) 0
                else
                    True

validPosComplete matrix x y = validPosComplete1 matrix [] x y x y (zoom matrix)
validPosComplete1 matrix adj x y countX countY zom = 
    if countY < y + 3
        then
            if countX == (x+1) && countY == (y+1)
                then
                    validPos1 matrix adj x y (countX) (countY+1) zom
                else 
                    validPos1 matrix (add adj (indexMatriz zom countX countY)) x y (countX) (countY+1) zom
        else 
            if countX < x+2
                then
                    validPos1 matrix adj x y (countX+1) 0 zom
                else
                    if (indexMatriz matrix x y) == (maxMatrix matrix) 
                        then
                            (contain adj ((maxMatrix matrix)-1))
                        else 
                            if (indexMatriz matrix x y) == 1 
                                then
                                    (contain adj 2)
                                else
                                    ((contain adj ((indexMatriz matrix x y)-1)) && (contain adj ((indexMatriz matrix x y)+1))) 

--Dado una posicion si solo tiene un adyacente accesible y su sucesor o antecesor no esta en el tablero entonces 
--lo asigna al accesible
adjUnique matrix x y =
    if (adjValues matrix x y) == 1
        then
            if contain (allValuesMatrix matrix) ((indexMatriz matrix x y)+1) == False
                then
                    if (indexMatriz matrix x y) /= (maxMatrix(matrix))
                        then
                            setInMatrix matrix ((indexMatriz matrix x y)+1) ((adjEmptyPos matrix x y)!!0) ((adjEmptyPos matrix x y)!!1)
                        else                
                            if (contain (allValuesMatrix matrix) ((indexMatriz matrix x y)-1)) == False
                                then
                                    if (indexMatriz matrix x y) /= 1
                                        then
                                            setInMatrix matrix ((indexMatriz matrix x y)-1) ((adjEmptyPos matrix x y)!!0) ((adjEmptyPos matrix x y)!!1)
                                        else
                                            matrix
                                else
                                    matrix            
                else                
                    if contain (allValuesMatrix matrix) ((indexMatriz matrix x y)-1) == False
                        then
                            if (indexMatriz matrix x y) /= 1
                                then
                                    setInMatrix matrix ((indexMatriz matrix x y)-1) ((adjEmptyPos matrix x y)!!0) ((adjEmptyPos matrix x y)!!1)
                                else
                                    matrix
                        else 
                            matrix            
        else 
            matrix                                    
    
--Dado una posicion y la matriz devuelve cuantos adyecentes accesibles(modificables) tiene
adjValues matrix x y = adjValues1 (zoom matrix) (x+1) (y+1) 0 0
adjValues1 matrix x y i result=
    if i < 8
        then
            adjValues1 matrix x y (i+1) ((indexMatriz (zeroIsOne matrix) (x + ((mov i) !!0)) (y + ((mov i)!!1)))+result) 
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

--Movimientos alrededor de una casilla
mov 0 = [-1,-1]
mov 1 = [-1,0]
mov 2 = [-1,1]
mov 3 = [0,-1]
mov 4 = [0,1]
mov 5 = [1,-1]
mov 6 = [1,0]
mov 7 = [1,1]


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

generateWithMatrix matrix = generateWithMatrix1 matrix 0 0
generateWithMatrix1 :: [[Int]]->Int-> Int->[[Int]]
generateWithMatrix1 matrix x y =
    --si al generar todos los unicos despues de editar la matrix se llega a todos los valores fijados de la anterior
    --pues se dice que la posicion que se elimino tiene valor unico ya que cambiando solo valores unicos sobre la misma se
    --llega a la solucion anterior
    if y < len(matrix!!x)
        then
            if (indexMatriz matrix x y) /= 1 &&
                (indexMatriz matrix x y) /= (maxMatrix matrix) &&
                (indexMatriz matrix x y) /= -1 &&
                (indexMatriz matrix x y) /= 0
                then
                    if (equalOrBest matrix (fullMatrixUniques((setInMatrix matrix 0 x y)))) == True
                        then
                            generateWithMatrix1 (setInMatrix matrix 0 x y) x (y+1)
                        else    
                            generateWithMatrix1 matrix x (y+1)
                else            
                    generateWithMatrix1 matrix x (y+1)
        else 
            if x < (len (matrix)-1)
                then
                    generateWithMatrix1 matrix (x+1) 0
                else
                    matrix

--Dado una matriz devuelve la lista de coordenadas de los valores distintos de cero, 1 y -1
boxCoor matrix = boxCoor1 matrix 0 (remove(remove(remove(remove (allValuesMatrix matrix) (-2)) 0)(maxMatrix matrix+1))1) []
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


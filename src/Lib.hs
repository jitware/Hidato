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
remove list x = if (contain list x) == True then remove_1 list x 0 else list
remove_1 list x pos = if pos == ((len list)-1) 
    then (if (index list pos) == x then removeFromPos list pos else list) 
    else (if (index list pos) == x then removeFromPos list pos else remove_1 list x (pos+1))

contain list x = x `elem` list -- devuelve en un booleano si la lista contiene el elemento x

--dado una lista l1 y una lista l2 elimina de l1 todos los elementos de l2
nand :: [Int] -> [Int] -> [Int]
nand l1 l2 = nand_1 l1 l2 0
nand_1 l1 l2 pos = if (pos+1) == (len l2) 
    then remove l1 (index l2 pos) 
    else nand_1 (remove l1 (index l2 pos)) l2 (pos+1)

--zoom Dado una matriz la rodea de un borde con todos los valores `-1` 
zoom :: [[Int]] -> [[Int]]
zoom matrix = zoom_1 matrix [[]] 0 0 0 0 
zoom_1 :: [[Int]] -> [[Int]] -> Int-> Int-> Int-> Int ->[[Int]] 
zoom_1 matrix matrix_result posX posY pos_X pos_Y = 
    if posY == (len(index matrix 0)+2) 
        then 
            if posX < (len(matrix)+1) 
                then
                    zoom_1 matrix (add matrix_result (index matrix_result 0)) (posX+1) posY pos_X pos_Y 
                else 
                    if pos_Y < (len(index matrix 0))
                        then 
                            zoom_1 matrix (setInMatrix matrix_result (indexMatriz matrix pos_X pos_Y) (pos_X+1) (pos_Y+1)) posX posY (pos_X) (pos_Y+1)
                        else
                            if pos_X < (len(matrix) -1)
                                then 
                                    zoom_1 matrix matrix_result posX posY (pos_X+1) (0)
                                else 
                                    matrix_result
        else    
            zoom_1 matrix (setInList matrix_result (-1:(index matrix_result 0)) 0) (posX) (posY+1) pos_X pos_Y

-- Elimina el zoom de una matriz
desZoom matrix = desZoom_1 matrix False 0
desZoom_1 matrix check x = 
    if check == False
        then
            desZoom_1 (removeFromPos (removeFromPos matrix ((len matrix)-1)) 0) True 0
        else
            if x < len(matrix)
                then
                    desZoom_1 (setInList matrix (remove(remove (index matrix x) (-1))(-1)) x) True (x+1)
                else
                    matrix    

--dado una posicion y una matriz devuelve todos los valores admisibles por esa posicion [caso general]
allValues :: [[Int]] -> Int -> Int -> [Int]
allValues matrix x y = allValues_1 matrix x y (zoom matrix)

allValues_1 :: [[Int]]-> Int-> Int ->[[Int]] -> [Int] 
-- z es una instancia del zoom de la matriz para no llamarla varias veces
allValues_1 matrix x y z = 
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

--Dado una matriz y una posicion devuelve los valores admisibles por esta posicion [comun]
admValues :: [[Int]] -> Int -> Int -> [Int]
admValues matrix x y = nand (allValues matrix x y) (allValuesMatrix matrix)

--Comprueba si la casilla esta entre dos diagonales de tal forma que solo puede tener un valor unico
uniqueDiag matrix x y = uniqueDiag_1 matrix x y (zoom matrix)
uniqueDiag_1 :: [[Int]] ->Int ->Int ->[[Int]]->[Int]
uniqueDiag_1 matrix x y zom =
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
uniqueInMid matrix x y = uniqueInMid_1 (zoom ( matrix)) (x+1) (y+1) 0 0
uniqueInMid_1 ::[[Int]] ->Int ->Int->Int->Int->[Int]
uniqueInMid_1 zom x y i j =
    if i < 8
        then
            if (abs((indexMatriz zom (x+((mov i)!!0)) (y+((mov i)!!1))) - (indexMatriz zom (x+((mov j)!!0)) (y+((mov j)!!1))))) == 2
                then  
                    if (indexMatriz zom (x+((mov i)!!0)) (y+((mov i)!!1))) == (-1) 
                        || (indexMatriz zom (x+((mov j)!!0)) (y+((mov j)!!1))) == (-1)
                        then
                            uniqueInMid_1 zom x y (i+1) j
                        else
                            if (communVecineEmpty zom 
                                (x+((mov i)!!0)) (y+((mov i)!!1)) (x+((mov j)!!0)) (y+((mov j)!!1))) == 1
                                then
                                    if (contain (allValuesMatrix zom)
                                        (((indexMatriz zom (x+((mov i)!!0)) (y+((mov i)!!1))) + (indexMatriz zom (x+((mov j)!!0)) (y+((mov j)!!1)))) `div` 2)) == True
                                        then
                                            uniqueInMid_1 zom x y (i+1) j
                                        else
                                            [((indexMatriz zom (x+((mov i)!!0)) (y+((mov i)!!1))) + (indexMatriz zom (x+((mov j)!!0)) (y+((mov j)!!1)))) `div` 2] 
                                else 
                                    uniqueInMid_1 zom x y (i+1) j
                else
                    uniqueInMid_1 zom x y (i+1) j
        else 
            if j < 7
                then
                    uniqueInMid_1 zom x y 0 (j+1)
                else
                    admValues zom x y    

--Devuelve la cantidad vecinos comunes que estan vacios
communVecineEmpty :: [[Int]]->Int->Int->Int->Int->Int
communVecineEmpty zom x1 y1 x2 y2 = communVecineEmpty_1 zom x1 y1 x2 y2 0 0

communVecineEmpty_1 :: [[Int]]->Int->Int->Int->Int->Int->Int->Int
communVecineEmpty_1 zom x1 y1 x2 y2 result i =
    if i < (len(communVecines x1 y1 x2 y2))
        then
            communVecineEmpty_1 
            zom x1 y1 x2 y2 
            (result + (zeroIsOne zom)!!((communVecines x1 y1 x2 y2)!!i!!0)!!((communVecines x1 y1 x2 y2)!!i!!1)) 
            (i+1)
        else
            result    

--Dada una matriz devuelve una matriz de 0 y 1 donde 0 es 1 y el resto es 0
zeroIsOne matrix = zeroIsOne_1 matrix 0 0
zeroIsOne_1 matrix x y =
    if y < (len (matrix!!x))
        then
            if (indexMatriz matrix x y) == 0
                then
                    zeroIsOne_1 (setInMatrix matrix 1 x y) x (y+1)
                else
                    zeroIsOne_1 (setInMatrix matrix 0 x y) x (y+1)
        else 
            if x < ((len matrix)-1)
            then
                zeroIsOne_1 matrix (x+1) 0
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
communVecines x y x1 y1 =  communVecines_1 x y x1 y1 [] 0
communVecines_1 x y x1 y1 result i = 
    if i < 8
        then
            if (contain (vecines x y) ((vecines x1 y1)!!i)) == True
                then
                    communVecines_1 x y x1 y1 (add result ((vecines x1 y1)!!i)) (i+1)
                else    
                    communVecines_1 x y x1 y1 result (i+1)
        else
            result            

endValues matrix x y = uniqueInMid matrix x y

--Dada una matriz devuelve el mayor valor de la misma 
maxMatrix matrix = maxMatrix_1 matrix [] 0
maxMatrix_1 matrix result y= 
    if y < (len matrix)
        then
            maxMatrix_1 matrix (add result (maximum (index matrix y))) (y+1)
        else
            maximum result    

--Dada una matriz con numeros devuelve si es valida la misma, si es alcanzable 
validMatrix matrix = validMatrix_1 matrix 0 0 
validMatrix_1 matrix x y= 
    if y < len(index matrix x)
        then
            validPos matrix x y && validMatrix_1 matrix x (y+1)
        else
            if x < ((len matrix) -1)
                then
                    validMatrix_1 matrix (x+1) 0
                else
                    True
--dada una posicion analiza si es posible que el valor de la misma sea el que tiene
validPos matrix x y = 
    if (indexMatriz matrix x y) == -1 ||(indexMatriz matrix x y) == 0
        then
            True
        else    
            validPos_1 matrix [] x y x y (zoom matrix)
validPos_1 matrix adj x y countX countY zom = 
    if countY < y + 3
        then
            if countX == (x+1) && countY == (y+1)
                then
                    validPos_1 matrix adj x y (countX) (countY+1) zom
                else 
                    validPos_1 matrix (add adj (indexMatriz zom countX countY)) x y (countX) (countY+1) zom
        else 
            if countX < x+2
                then
                    validPos_1 matrix adj x y (countX+1) 0 zom
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
validMatrixComplete matrix = validMatrixComplete_1 matrix 0 0
validMatrixComplete_1 matrix x y =
    if y < len(index matrix x)
        then
            validPosComplete matrix x y && validMatrixComplete_1 matrix x (y+1)
        else
            if x < ((len matrix) -1)
                then
                    validMatrixComplete_1 matrix (x+1) 0
                else
                    True

validPosComplete matrix x y = validPosComplete_1 matrix [] x y x y (zoom matrix)
validPosComplete_1 matrix adj x y countX countY zom = 
    if countY < y + 3
        then
            if countX == (x+1) && countY == (y+1)
                then
                    validPos_1 matrix adj x y (countX) (countY+1) zom
                else 
                    validPos_1 matrix (add adj (indexMatriz zom countX countY)) x y (countX) (countY+1) zom
        else 
            if countX < x+2
                then
                    validPos_1 matrix adj x y (countX+1) 0 zom
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
adjValues matrix x y = adjValues_1 (zoom matrix) (x+1) (y+1) 0 0
adjValues_1 matrix x y i result=
    if i < 8
        then
            adjValues_1 matrix x y (i+1) ((indexMatriz (zeroIsOne matrix) (x + ((mov i) !!0)) (y + ((mov i)!!1)))+result) 
        else 
            result    

--Dado una posicion devuelve una posicion adyacente vacia
adjEmptyPos :: [[Int]]-> Int -> Int ->[Int]
adjEmptyPos matrix x y = adjEmptyPos_1 (zoom matrix) (x+1) (y+1) 0
adjEmptyPos_1 matrix x y i =
    if i < 8
        then
            if (indexMatriz (matrix) (x + ((mov i)!!0)) (y + ((mov i)!!1))) == 0
                then
                    [(x + ((mov i)!!0) -1),(y + ((mov i)!!1)-1)]
                else
                    adjEmptyPos_1 matrix x y (i+1)
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
fullMatrixUniques matrix = fullMatrixUniques_1 matrix True
fullMatrixUniques_1 matrix check =
    if check == True
        then
            fullMatrixUniques_2 matrix 0 0 0
        else
            matrix    
fullMatrixUniques_2 matrix x y changes=
    if y < len(matrix!!x)
        then
            if (indexMatriz matrix x y) /= 0
                then
                    if (matrix) /= (adjUnique matrix x y) --si habra cambio hacer el cambio
                        then
                            fullMatrixUniques_2 (adjUnique matrix x y) x (y+1) (changes+1)
                        else    
                            fullMatrixUniques_2 matrix x (y+1) changes
                else
                    if(len(endValues matrix x y))==1 && (indexMatriz matrix x y) == 0 && ((endValues matrix x y)!!0) /= (-1)
                        then
                            fullMatrixUniques_2 (setInMatrix matrix ((endValues matrix x y)!!0) x y) x (y+1) (changes+1)
                        else    
                            fullMatrixUniques_2 matrix x (y+1) changes
        else
            if x < (len(matrix)-1)
                then
                    fullMatrixUniques_2 matrix (x+1) 0 changes
                else 
                    fullMatrixUniques_1 matrix (changes > 0)   

--Busqueda en profundidad para encontrar una respuesta valida dado una matriz
dfS matrix = dFS (zoom matrix) ((posUno (zoom matrix))!!0) ((posUno (zoom matrix))!!1) 0
dFS matrix x y i =
    -- si i es menor que 8
    if i<8
        then
            -- si la casilla vecina dada tiene valor sucesor del valor actual...
            if (indexMatriz matrix (x + (mov i)!!0) (y + (mov i)!!1)) == ((indexMatriz matrix x y)+1)
                then 
                    --Cambia de posicion al adyacente con el valor dado
                    dFS 
                    matrix -- la matriz se mantiene igual
                    (x + (mov i)!!0) -- x cambia de posicion x al adyacente
                    (y + (mov i)!!1) -- y cambia de posicion y al adyacente
                    0 -- i = 0
                else
                    -- si la casilla vecina dada tiene valor cero...
                    if (indexMatriz matrix (x + (mov i)!!0) (y + (mov i)!!1)) == 0
                        then
                            --si el la casilla admite el valor del sucesor(esta contenida en los valores admitidos) ...
                            if (contain (endValues matrix (x + (mov i)!!0) (y + (mov i)!!1)) ((indexMatriz matrix x y)+1)) == True
                                then
                                    --Cambia de posicion al adyacente con el valor dado
                                    dFS 
                                    (setInMatrix matrix -- la matriz cambia
                                    ((indexMatriz matrix x y)+1) -- valor sucesor se agraga a la posicion adyacente que era cero 
                                    (x + (mov i)!!0)  -- posicion x del adyacente
                                    (y + (mov i)!!1)) -- posicion y del adyacente
                                    (x + (mov i)!!0) -- x cambia de posicion x al adyacente
                                    (y + (mov i)!!1) -- y cambia de posicion y al adyacente
                                    0 -- i = 0
                                else
                                    --Aumenta el valor de i
                                    dFS 
                                    matrix -- la matriz se mantiene igual
                                    x -- x se mantiene igual
                                    y -- y se mantiene igual
                                    (i + 1) -- i +=1
                        else
                            --Aumenta el valor de i
                            dFS 
                            matrix -- la matriz se mantiene igual
                            x -- x se mantiene igual
                            y -- y se mantiene igual
                            (i + 1) -- i +=1
        else
            if (validMatrixComplete matrix) == True
                then
                    desZoom matrix
                else
                    desZoom matrix-- aqui deberia continuar la recursividad

--Devuelve se la matriz resultante poseen al menos todos los valores fijos de la anterior
equalOrBest matrix matrix_result = equalOrBest_1 matrix matrix_result 0 0
equalOrBest_1 matrix matrix_result x y =
    if y < len(matrix!!x)
        then
            if ((indexMatriz matrix x y) /= 0) && ((indexMatriz matrix x y) /= -1)
                then
                    (indexMatriz matrix x y) == (indexMatriz matrix_result x y) &&
                    equalOrBest_1 matrix matrix_result x (y+1)
                else
                    equalOrBest_1 matrix matrix_result x (y+1)    
        else
            if x<(len(matrix)-1)
                then
                    equalOrBest_1 matrix matrix_result (x+1) 0
                else
                    True        


generateWithMatrix matrix = generateWithMatrix_1 matrix 0 0
generateWithMatrix_1 :: [[Int]]->Int-> Int->[[Int]]
generateWithMatrix_1 matrix x y =
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
                            generateWithMatrix_1 (setInMatrix matrix 0 x y) x (y+1)
                        else    
                            generateWithMatrix_1 matrix x (y+1)
                else            
                    generateWithMatrix_1 matrix x (y+1)
        else 
            if x < (len (matrix)-1)
                then
                    generateWithMatrix_1 matrix (x+1) 0
                else
                    matrix


--Dado una matriz devuelve la lista de coordenadas de los valores distintos de cero, 1 y -1
boxCoor matrix = boxCoor_1 matrix 0 (remove(remove(remove(remove (allValuesMatrix matrix) (-2)) 0)(maxMatrix matrix+1))1) []
boxCoor_1 matrix i all result=
    if i < len(all)
        then
            boxCoor_1 matrix (i+1) all (add result (posValue (all!!i) matrix))
        else 
            result

--Devuelve la posicion donde se encuentra el valor uno en la matriz
posUno matrix = posValue 1 matrix

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
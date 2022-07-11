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
allValuesMatrix matrix = allValuesMatrix_1 matrix [0,1,-1,-2] 0 0 
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
            if (indexMatriz zom (x+1+1) (y-1+1)) == -1 || (indexMatriz zom (x+1+1) (y-1+1)) == 0
                || (indexMatriz zom (x-1+1) (y+1+1)) == -1 || (indexMatriz zom (x-1+1) (y+1+1)) == 0
                then
                    admValues matrix x y    
                else    
                    [(((indexMatriz zom (x+1+1) (y-1+1))+(indexMatriz zom (x-1+1) (y+1+1)))`div`2)]
        else
            if abs((indexMatriz zom (x-1+1) (y-1+1)) - (indexMatriz zom (x+1+1) (y+1+1))) == 2
                then
                    [((indexMatriz zom (x-1+1) (y-1+1)) + (indexMatriz zom (x+1+1) (y+1+1))`div`2)]
                else
                    admValues matrix x y    

endValues matrix x y = uniqueDiag matrix x y

--Dada una matriz devuelve el mayor valor de la misma 
maxMatrix matrix = maxMatrix_1 matrix [] 0
maxMatrix_1 matrix result y= 
    if y < (len matrix)
        then
            maxMatrix_1 matrix (add result (maximum (index matrix y))) (y+1)
        else
            maximum result    

--Dada una matriz llena de numeros devuelve si es valida la misma, si es alcanzable 
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

--aplica DFS y pasa por todas las soluciones posibles acotado esto por las opciones unicas

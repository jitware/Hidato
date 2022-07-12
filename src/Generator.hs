import Lib 
import Solver

generateWithMatrix matrix = generateWithMatrix1 matrix 0 0 (maxMatrix matrix)
generateWithMatrix1 :: [[Int]]->Int-> Int->Int->[[Int]]
generateWithMatrix1 matrix x y maxM=
    --si al generar todos los unicos despues de editar la matrix(una ronda) se llega a todos los valores fijados de la anterior
    --pues se dice que la posicion que se elimino tiene valor unico ya que cambiando solo valores unicos sobre la misma se
    --llega a la solucion anterior
    if y < len(matrix!!x)
        then
            if (indexMatriz matrix x y) /= 1 &&
                (indexMatriz matrix x y) /= maxM &&
                (indexMatriz matrix x y) /= -1 &&
                (indexMatriz matrix x y) /= 0
                then
                    if (indexMatriz matrix x y)== (indexMatriz (roundMatrixUniques (setInMatrix matrix 0 x y)) x y)
                        then
                            generateWithMatrix1 (setInMatrix matrix 0 x y) x (y+1) maxM
                        else    
                            generateWithMatrix1 matrix x (y+1) maxM
                else            
                    generateWithMatrix1 matrix x (y+1) maxM
        else 
            if x < (len (matrix)-1)
                then
                    generateWithMatrix1 matrix (x+1) 0 maxM
                else
                    matrix

generate semilla = generateWithMatrix(snd(solve(semilla)))
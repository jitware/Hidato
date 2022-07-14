module Main where
import Matrix
import Generator
import Solver
import Lib
import System.Environment

main :: IO ()
main = 
    do 
        -- matrix <- getArgs
        let matrix = [[0,  0, 0,  0,  0 ],[1,  0, 0,  0, -1 ],[0,  0, 0,  0,  0 ],[0,  0, 0,  0, -1 ],[0,  0, 0,  0, -1 ],[0,  0, 28,  0, -1 ],[0, -1,-1,  0, -1 ]]
            generado = generate matrix
            solucion = snd(solve generado)
        print "Tablero Generado:"
        print generado
        print "Solucion:"
        print solucion


strToMatrix1 str result tempInt tempArray i =
    if i <  (length(str)-1)
        then
            if str!!i == "["
                then 
                    strToMatrix1 str (result ++ [tempArray]) tempInt [] (i+1)
                else 
                    if str!!i == "," && str!!(i-1) == "]"
                        then
                            strToMatrix1 str result 0 (tempArray++[tempInt]) (i+1)
                        else 
                            if str!!i /= "," && str!!i /= "]"
                                then
                                    strToMatrix1 str result 1 tempArray (i+1)
                                else    
                                    strToMatrix1 str result tempInt tempArray (i+1)
        else
            result                        

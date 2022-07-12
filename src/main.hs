module Main(main) where
import Lib
import Matrix
import Solver

main :: IO() 
main =
    let matrix = [[0  ,33 ,35 ,0  ,0  ,-1 ,-1 ,-1 ], [0  ,0  ,24 ,22 ,0  ,-1 ,-1 ,-1 ], [0  ,0  ,0  ,21 ,0  ,0  ,-1 ,-1 ], [0  ,26 ,0  ,13 ,40 ,11 ,-1, -1 ], [27 ,0  ,0  ,0  ,9  ,0  ,1  ,-1 ], [-1 ,-1 ,0  ,0  ,18 ,0  ,0  ,-1 ], [-1 ,-1 ,-1 ,-1 ,0  ,7  ,0  ,0  ], [-1 ,-1 ,-1 ,-1 ,-1 ,-1 ,5  ,0  ]];
           boxes = ord_boxes matrix (boxCoor matrix);
           current = posUno matrix;
           end = boxes!!0
           print (backtraking current end boxes matrix)

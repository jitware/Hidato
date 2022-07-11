import Matrix

myfold_bool f matrix ys xs  end value boxes  = aux matrix ys xs  end value boxes
  where  aux matrix ys []  end value boxes     = ys
        --  aux matrix ys _  end value boxes = ys
         aux matrix ys (x:xs)  end value boxes = aux matrix (f matrix ys x end value boxes) xs  end value boxes


func neigbor end boxes []=
    if neigbor!!0 == 2 && neigbor!!1 ==0
        then (True, [neigbor])
    else 
        (False, [])


f_back matrix acc neigbor end value boxes = 
    if (fst acc) == True
        then acc
        else func neigbor end boxes []

loe matrix = myfold_bool f_back matrix (False, []) [[1,0],[2,0],[3,0]]  [] 1 []

fun1 num =  (True, _ ) == (True, [0])
import Lib

index matrix cord = matrix!!(cord!!0)!!(cord!!1)

myfold f matrix ys xs current = aux matrix ys xs current
  where  aux matrix ys [] current     = ys
         aux matrix ys (x:xs) current = aux matrix (f matrix ys x current) xs current

-- For para el backtraking 
--- args:
---- f: Funcion a ejecutar parametros del for
---- matrix: Tablero
---- ys: Variable acumulada en el for en este caso un False o True
---- xs: La lista que se va a iterar
myfold_bool f matrix ys xs  end value boxes  = aux matrix ys xs  end value boxes
  where  aux matrix ys []  end value boxes     = ys
         aux matrix [True, _] _  end value boxes = [True, matrix]
         aux matrix ys (x:xs)  end value boxes = aux matrix (f matrix ys x end value boxes) xs  end value boxes

is_valid_cord  x y matrix = (x < length matrix) && (y <  length (matrix!!0)) && (x>=0) && (y>=0)

valuex x y n matrix = (matrix!!(x)!!(y)==0) || (matrix!!(x)!!(y)==n)

--Validar una coordenada segun la tabla
---args:
--- current: [[x,y]] donde x e y son las coordenadas
--- n: El valor posible de la siguiente casilla
--- matrix: Tablero
valid_cord current n matrix = 
    let x = current!!0!!0 ; y = current!!0!!1 ; 
        in  if (is_valid_cord x y matrix) && (valuex x y n matrix)
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

f_back matrix acc neigbor end value boxes = 
    backtraking neigbor end boxes (setInMatrix matrix value (neigbor!!0) (neigbor!!1))

backtraking current end boxes matrix = 
    -- Valor de table[current_x][current_y]==table[end_x][end_y] en ese caso
    -- Comprueba que tengan la misma coordena sino return false
    -- En caso de tenerla si hay mas casilla actualiza current y end y entra otra vez
    -- Sino es la solucion 
    -- current = [x,y]  end = [t,z]
    if index matrix current  == index matrix end
        then if current == end
            then 
                let t_last = last_box boxes 
                    in if current == t_last
                        then [True, matrix]
                         else let t_next = next_box current boxes
                                in backtraking end t_next boxes matrix
        
            else [False, []]
        -- dame los vecinos
        else let neigbors = valid_neigbors current matrix
                in myfold_bool f_back matrix [False, []] neigbors end ((index matrix current)+1) boxes

-----------------------------------------------
-- f_back matrix acc neigbor end value boxes = 
--     backtraking neigbor end boxes (setInMatrix matrix value neigbor!!0 neigbor!!1) || False
---  a = [[1,0,0,0],[0,0,0,0],[6,0,0,0]]
---- current = [0,0]
---- end = [2,0]
---- boxes = [[2,0]]
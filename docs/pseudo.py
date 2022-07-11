from typing import List


def backtracking(current_x, current_y, end_x, end_y, table, boxes:List):
    # a =  table[current_x][current_y]
    # b =  table[end_x][end_y]
    # print(current_x,current_y)
    # print_matrix(table)
    if table[current_x][current_y]==table[end_x][end_y]:
    
        if current_x == end_x and current_y == end_y:
            last = -1
            last_x, last_y = boxes[last]
            if current_x == last_x and current_y == last_y:
                return True
            else:
                next =  boxes.index((end_x, end_y)) + 1
                end_x, end_y  = boxes[next]
                return backtracking(current_x, current_y, end_x, end_y, table, boxes)
        else:
            return False 
         
    neigbors = valid_neigbors(current_x, current_y, table,)
    for neigbor_x, neigbor_y in neigbors:
        if table[neigbor_x][ neigbor_y]==0:
            table[neigbor_x][ neigbor_y] = table[current_x][ current_y]+1
        
        if backtracking(neigbor_x, neigbor_y, end_x, end_y, table, boxes):
            return True
        table[neigbor_x][neigbor_y]=0
        
    return False

def  valid_neigbors(cord_x, cord_y, table,):
    neigbors = []
    n= table[cord_x][cord_y]+1
    for x,y in [(1,0),(1,1),(1,-1),(0,1),(0,-1),(-1,0),(-1,1),(-1,-1)]:
        new_x, new_y = cord_x+x,cord_y+y
        if is_valid_cord(new_x, new_y,table) and (table[new_x][new_y]==0 or table[new_x][new_y]==n):
            neigbors.append((new_x, new_y ))
    return neigbors

def is_valid_cord(x,y,a):
    if x <len(a) and y <len(a[len(a)-1]) and x>=0 and y>=0:
        return True
 
def print_matrix(a): 
    print()  
    print('\n'.join(['  '.join(['{:2}'.format(item) for item in row])
        for row in a]))
    
a = [[0,    33,   35,   0,    0,  '.',  '.',   '.'],
     [0,    0,    24,   22,   0,  '.',  '.',   '.'],
     [0,    0,    0,    21,   0,  0,    '.',   '.'],
     [0,    26,   0,    13,   40, 11,   '.',   '.'],
     [27,   0,    0,    0,    9,  0,    1,     '.'],
     ['.',  '.',  0,    0,    18, 0,    0,     '.'],
     ['.',  '.',  '.',  '.',  0,  7,    0,     0  ],
     ['.',  '.',  '.',  '.',  '.','.',  5,     0  ],
     ]
casillas =[]
for i in range(len(a)):
    for j in range(len(a[len(a)-1])):
        if a[i][j]!= '.' and a[i][j]>1:
            casillas.append((i,j))
            
casillas.sort(key = lambda x: a[x[0]][x[1]])
print_matrix(a)
print(casillas)

backtracking(7, 6, 3, 4, a, casillas)

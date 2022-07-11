
def valid(x,y, a):
    num_x = len(a)
    num_y = len(a[num_x-1])
    
    if x <len(a) and y <len(a[len(a)-1]) and x>=0 and y>=0:
        return True

a = [[1,0,0,0],[0,0,0,0],[6,0,0,0]]
print('\n'.join([' '.join(['{:4}'.format(item) for item in row])
    for row in a]))


f_x = 0
f_y=0

neigbors = []

for x,y in [(1,0),(1,1),(1,-1),(0,1),(0,-1),(-1,0),(-1,1),(-1,-1)]:
    if valid(f_x+x,f_y+y, a):
       neigbors.append((f_x+x,f_y+y ))

print()

for x,y in neigbors:
    a[x][y]= a[0][0] +1
    
print()  
print('\n'.join([' '.join(['{:4}'.format(item) for item in row])
    for row in a]))

def se(a):
    a[0][0]=2

se(a) 
print()  
print('\n'.join([' '.join(['{:4}'.format(item) for item in row])
    for row in a]))
import numpy as np

# 2 cells 5 balls
w = np.array(((1,0),
              (0.5,0.5),
              (0,1),
              (0.5,0),
              (0.0,0.5)))

# ball velocity comp
u = np.array(((1,2,0),
              (1,0,3),
              (0,2,3),
              (4,5,6),
              (7,8,9)))

# ball volumes
v = np.array((0.8, 0.9, 0.7, 0.2, 0.2))

# volume fraction
vf = (w.T*v).sum(axis=1)

# equation 3.7 which is the average particle velocity in each cell
uc = np.einsum('ik,ij,i',w,u,v)/vf

print uc

res = []
for c in w.T:
    res.append(v*(c*u.T[0], c*u.T[1], c*u.T[2]))
#r=np.array(res)

for r, volf in zip(res,vf):
    print np.sum(r,axis=1)/volf

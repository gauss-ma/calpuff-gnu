#!/usr/bin/python

import sys
import numpy as np

xmin=float(sys.argv[1])
xmax=float(sys.argv[2])
nx=int(sys.argv[3])
ymin=float(sys.argv[4])
ymax=float(sys.argv[5])
ny=int(sys.argv[6])


x=np.linspace(xmin,xmax,nx)
y=np.linspace(ymin,ymax,ny)

X,Y=np.meshgrid(x,y)

out=np.column_stack((X.ravel(),Y.ravel()))
np.savetxt(sys.stdout, out, fmt='%.3f %.3f')

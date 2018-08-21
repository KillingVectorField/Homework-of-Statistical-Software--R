from scipy import integrate
import numpy as np

def f0(x,y):
    return (np.exp(-45 * (x + 0.4) ** 2 - 60 * (y - 0.5) ** 2)+ 
            0.5*np.exp(-90*(x-0.5)**2-45*(y+0.1)**4))

result=integrate.dblquad(f0,-1,1,lambda x:-1, lambda x:1)
print(result)

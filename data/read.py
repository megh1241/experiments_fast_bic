import numpy as np
import matplotlib.pyplot as plt

a = np.zeros((501, 5001))
b = np.zeros((501, 5001))
c = np.zeros((501, 5001))
i = 0
j = 0
with open('conf2-out.txt', 'r') as the_file:
    for line in the_file:
        newline = line.split()
        num1 = float(newline[0])
        num2 = float(newline[1])
        a[i:i+50, j:j+50] = num1
        b[i:i+50, j:j+50] = num2
        print (num1)
        print (num2)
        if num2 > 0:
            c[i:i+50, j:j+50] = (float)(num1/num2)
        else:
            c[i:i+50, j:j+50] = 0
        
        if j>=4950 :
                j = 0
                i+=50
        else:
            j+=50
plt.imshow(c)
ax = plt.gca()            
plt.colorbar()
ax.set_aspect(5)
plt.show()
print (c)        


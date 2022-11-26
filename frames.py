from matplotlib import pyplot as plt
import numpy as np
import cv2

vidcap = cv2.VideoCapture('c:/Users/jfurtney/Dropbox/home/48_Caspers_Hill_Road/6e59cf3e8ddbe75708d5146838210d98.MP4')
success, image = vidcap.read()
image = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
data = np.zeros_like(image)
i=0
while success:
    success, image = vidcap.read()
    image = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
    if i in range(69,77):
        data = np.maximum(data, image)
    if i == 78:
        break
    #cv2.imwrite("caps/frame%d.jpg" % i, image)
    i+=1

plt.imshow(data, cmap="gray")
plt.show()

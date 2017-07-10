import threading
from time import sleep

def update_GUI():
    print "updaing gui"
    sleep(0.25)

def computationally_intense_task():
    total=0
    for i in range(5):
        print "intermediate calculation result {}".format(total)
        for j in range(10000):
            for k in range(10000):
                total += 1

t0 = threading.Thread(target=computationally_intense_task)
t0.daemon = True
t0.start()

while t0.is_alive():
    update_GUI()

print "done"

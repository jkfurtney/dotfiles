import pyttsx
import random

engine = pyttsx.init()

def say_and_read(data):
    engine.say(data)
    engine.runAndWait()
    return raw_input()

while True:
    n1 = random.randrange(1,20)
    n2 = random.randrange(1,20)

    while True:
        res = say_and_read("{} plus {}".format(n1, n2))

        intres = True
        try: int(res)
        except ValueError:  intres = False

        if intres: break

    if int(res) == n1 + n2:
        print "yes"
    else:
        print "no {} + {} = {}".format(n1,n2,n1+n2)

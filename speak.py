import pyttsx

engine = pyttsx.init()

def say(data):
    engine.say(data)
    engine.runAndWait()

while True:
    say(raw_input())

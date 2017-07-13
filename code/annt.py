import time
import pyttsx
engine = pyttsx.init()

def say(data):
    engine.say(data)
    engine.runAndWait()

numbers=['zero','one','two','three','four', 'five','six','seven','eight','nine','ten','eleven','twelve']

def say_time():
    s = time.localtime()
    h, m = s.tm_hour, s.tm_min
    if m == 30:
        suffix = " o clock "
    else:
        suffix = " thirty "
    announcement = " It is {hour} {suffix}".format(hour=str(h), suffix=suffix)
    say(announcement)

import schedule
schedule.every().hour.at(":00").do(say_time)
schedule.every().hour.at(":30").do(say_time)

while True:
    schedule.run_pending()
    time.sleep(5)

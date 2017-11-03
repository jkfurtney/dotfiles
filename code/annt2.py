import time

import pyttsx

engine = pyttsx.init()

def say(data):
    engine.say(data)
    engine.runAndWait()

header = "It is "
oclock = " o clock "
thirty = " thirty "
fifteen= " fifteen "

numbers=['zero','one','two','three','four', 'five','six','seven','eight','nine','ten','eleven','twelve']



def say_time():
    s=time.localtime()
    h, m = s.tm_hour, s.tm_min

    if h>12:
        h-=12

    if m>=45:
        h+=1
        m=0;
    elif m<15:
        m=0
    if m>15 and m<45:
        m=30
    else:
        m=0
    if m == 0:
        suffix = oclock
    else:
        suffix = thirty

    announcement = "{header} {hour} {suffix}".format(header=header,
                                                     hour=str(h),
                                                     suffix=suffix)
    say(announcement)

import schedule

schedule.every().hour.at(":00").do(say_time)
schedule.every().hour.at(":30").do(say_time)

while True:
    schedule.run_pending()
    time.sleep(1)

# Facts:
# User Python code must be execuited in the main thread.
# Qt update code must be execuited in the main thread.
# We want to update the GUI as user code runs.
# We want to be able to interupt user code

import threading
from time import sleep
import Queue
import traceback

class CodeEvaluator(threading.Thread):
    def __init__(self):
        threading.Thread.__init__(self)
        self.setDaemon(True)
        self.q = Queue.Queue(maxsize=1)

    def evaluate_code(self, code): # this runs in the main thread
        print "evaling", code
        event = threading.Event()
        self.q.put(event) # tell poll thread to start

        def ee_compile(code, src=''):
          try:
            return compile(code, src, 'eval')
          except SyntaxError:
            return compile(code, src, 'exec')

        try:
            eval(ee_compile(code), globals())
        except Exception, data:
            print traceback.format_exc()

        event.set() # tell the poll thread to stop

    def run(self):  # only this method runs in the worker thread.

        while True:
            evaluation_event = self.q.get()  # blocking read
            print "heartbeat thread awake"
            # loop while user code in being evaluated.
            while not evaluation_event.is_set():
                itasca.async_poll()
                print "updating gui"
                sleep(0.05)

            self.q.task_done()
            print "heartbeat thread going to sleep"



def countDown(num):  # computationally expensive function (test)
    total = 0
    while (num > 0):
        total += num
        num -= 1
    print "the final result is: ", total


if name == '__main__':

    evaluator = CodeEvaluator()
    evaluator.start()

    evaluator.evaluate_code("countDown(10000000)")
    evaluator.evaluate_code("countDown(12345678)")
    # test handling a syntax error
    evaluator.evaluate_code("derp derp derp")
    # test handling a runtime error
    evaluator.evaluate_code("1/0")

    evaluator.evaluate_code("a=10")
    print "a is", a

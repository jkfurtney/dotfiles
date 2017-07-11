import threading
from time import sleep
import Queue
import traceback

class CodeEvaluationThread(threading.Thread):
    def __init__(self):
        threading.Thread.__init__(self)
        self.setDaemon(True)
        self.running = False
        self.q = Queue.Queue(maxsize=1)

    def evaluate_code(self, code, update_msg): # this runs in the main thread
        self.running = True
        self.q.put(code)
        while self.running:
            print update_msg
            sleep(0.25)

    def run(self):  # only this method runs in the worker thread.
        def ee_compile(code, src=''):
          try:
            return compile(code, src, 'eval')
          except SyntaxError:
            return compile(code, src, 'exec')
        while True:
            self.running = False
            code = self.q.get()  # blocking read
            self.running = True

            try:
                eval(ee_compile(code), globals())
            except Exception, data:
                print traceback.format_exc()

            self.q.task_done()


def countDown(num):  # computationally expensive function (test)
    total = 0
    while (num > 0):
        total += num
        num -= 1
    print "the final result is: ", total


if __name__ == '__main__':

    evaluator = CodeEvaluationThread()
    evaluator.start()

    evaluator.evaluate_code("countDown(10000000)", "part 1")
    evaluator.evaluate_code("countDown(12345678)", "part 2")
    # test handling a syntax error
    evaluator.evaluate_code("derp derp derp", "part 3")
    # test handling a runtime error
    evaluator.evaluate_code("1/0", "part 3")

    evaluator.evaluate_code("a=10", "part 3")
    print "a is", a

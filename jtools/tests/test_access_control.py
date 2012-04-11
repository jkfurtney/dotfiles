from jtools import access_control

class test_class(access_control):
    def __init__(self):
        self.james = 2
        self.lock()


def test_access_control_behavior():
    tc = test_class()
    assert tc.is_locked() == True
    tc.unlock()
    assert tc.is_locked() == False
    tc.unlock()
    assert tc.is_locked() == False
    tc.lock()
    tc.lock()
    assert tc.is_locked() == True


def test_access_control_exceptions():
    tc = test_class()
    tc.james = tc.james + 1
    assert tc.james == 3

    try:
        tc.simon = 0
    except Exception as ex:
        assert Exception.__name__ == ex.__class__.__name__
        assert ex.args[0] == "No existing attribute: simon"
    else:
        raise Exception("Expected an exception")

    assert not hasattr(tc, "simon")
    tc.unlock()
    tc.simon = 0
    assert tc.simon == 0
    tc.lock()

    assert tc.simon == 0
    assert tc.james == 3

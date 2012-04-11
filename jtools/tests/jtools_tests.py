from nose.tools import *
import jtools

def setup():
    """Setup tests."""
    pass

def teardown():
    """Tear down tests."""
    pass

def test_get_version():
    assert type(jtools.get_version()) == str

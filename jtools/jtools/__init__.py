#!/usr/bin/env python

VERSION = (0, 0, 1, 'alpha', 1)

def get_version(version=None):
    """
    Derives a PEP386-compliant verison number
    from VERSION.
    """
    if version is None:
        version = VERSION
    assert len(version) == 5
    assert version[3] in ("alpha", "beta", "rc", "final")

    parts = 2 if version[2] == 0 else 3
    main = ".".join(str(digit) for digit in version[:parts])

    sub = ""
    if version[3] != "final":
        mapping = {"alpha": "a", "beta": "b", "rc": "rc"}
        sub = mapping[version[3]] + str(version[4])

    return main + sub


from access_control import access_control
from report import latex_report
from main import prefix

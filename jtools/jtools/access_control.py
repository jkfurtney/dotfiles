

class access_control(object):
    """ A simple attribute setting access control mechanism: after
    calling self.lock() new attribtes cannot be set but old
    attributes can still be given a new value. Calling self.unlock()
    allows new attributes to be assigned again

    This class was created to stop client code from adding new
    attributes unless they really mean it. I have had several bugs
    where a typo caused a new instance attribute to be created when
    the intention was to modify an existing attribute.
    """
    def __setattr__(self, key, value):
        if not hasattr(self, "_lock_attributes"):
            self.__dict__[key] = value
        else:
            if hasattr(self, key):
                self.__dict__[key] = value
            else:
                raise Exception("No existing attribute: %s" % key)

    def is_locked(self):
        """
        returns True if setting new attributes in locked
        """
        return hasattr(self, "_lock_attributes")

    def lock(self):
        """
        after calling this function new class attributes can not be
        assigned. Call unlock() to reverse
        """
        if not self.is_locked():
            self._lock_attributes = True

    def unlock(self):
        if self.is_locked():
            del(self._lock_attributes)


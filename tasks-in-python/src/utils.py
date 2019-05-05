import re


class ScopedGlobals:
    class _Sentinel:
        pass

    def __init__(self, handle=None, **assignments):
        self._handle = handle
        self._before = {}
        for (name, value) in assignments.items():
            self._before[name] = globals().get(name, self._Sentinel)
            globals()[name] = value

    def __enter__(self):
        return self._handle

    def __exit__(self, *args, **kwargs):
        for (name, value) in self._before.items():
            if value is self._Sentinel:
                del globals()[name]
            else:
                globals()[name] = value


class TaggedTuple:
    def __init__(self, items=None):
        self._items = items or {}

    def values(self):
        return self._items.values()

    def __getattr__(self, name):
        return self._items[name]

    def __setattr__(self, name, value):
        if not name == '_items':
            raise NotImplementedError('Cannot mutate {}'.format(
                self.__class__.__name__))
        super().__setattr__(name, value)

    def __repr__(self):
        return repr(self._items)


def freeze(**items):
    return TaggedTuple(items)


def camel_to_kebab(word):
    word = re.sub(r"([A-Z]+)([A-Z][a-z])", r'\1-\2', word)
    word = re.sub(r"([a-z\d])([A-Z])", r'\1-\2', word)
    word = word.replace("_", "-")
    return word.lower()


x = freeze(foo=1, bar='baz')

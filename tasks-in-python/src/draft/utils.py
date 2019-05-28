import re


class ScopedGlobal:
    def __init__(self, namespace, var_name, value):
        self._var_name = var_name
        self._value = value
        self._namespace = namespace

    def __enter__(self):
        # pylint: disable=attribute-defined-outside-init
        if self._var_name in self._namespace:
            self._overwritten_value = self._namespace[self._var_name]
            self._namespace[self._var_name] = self._value
        else:
            self._namespace[self._var_name] = self._value
        return self._value

    def __exit__(self, exc_type, exc_value, exc_traceback):
        if hasattr(self, '_overwritten_value'):
            self._namespace[self._var_name] = self._overwritten_value
        else:
            del self._namespace[self._var_name]


def camel_to_kebab(word):
    word = re.sub(r"([A-Z]+)([A-Z][a-z])", r'\1-\2', word)
    word = re.sub(r"([a-z\d])([A-Z])", r'\1-\2', word)
    word = word.replace("_", "-")
    return word.lower()


class TaggedTuple:
    def __init__(self, items=None):
        self._items = items or {}

    def values(self):
        return self._items.values()

    def __getattr__(self, name):
        return self._items[name]

    def __setattr__(self, name, value):
        if not name == '_items':
            raise NotImplementedError('Cannot mutate {}'.format(self.__class__.__name__))
        super().__setattr__(name, value)

    def __repr__(self):
        return repr(self._items)


def freeze(**items):
    return TaggedTuple(items)

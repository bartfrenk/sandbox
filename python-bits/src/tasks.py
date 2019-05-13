from collections import namedtuple
from abc import ABC, abstractmethod


class TaskDescriptor(ABC):

    Schema = namedtuple('TaskSchema', ['inputs', 'outputs'])

    @property
    def schema(self):
        pass

    def connect(self, input_descriptors):
        pass

    def __call__(self, **input_descriptors):
        self.connect(input_descriptors)


class DataDescriptor:
    def schema(self):
        pass


_GLOBAL_TASK_GRAPH_VAR = '_TASK_GRAPH'


class ScopedGlobal:
    def __init__(self, var_name, value):
        self._var_name = var_name
        self._value = value

    def __enter__(self):
        # pylint: disable=attribute-defined-outside-init
        if self._var_name in globals():
            self._overwritten_value = globals()[self._var_name]
            globals()[self._var_name] = self._value
        else:
            globals()[self._var_name] = self._value
        return self._value

    def __exit__(self, exc_type, exc_value, exc_traceback):
        if hasattr(self, '_overwritten_value'):
            globals()[self._var_name] = self._overwritten_value
        else:
            del globals()[self._var_name]


class TaskGraph(TaskDescriptor):
    """
    Immutable representation of an abstract work flow.  The representation is in
    terms of a directed bipartite graph, consisting of TaskSchema nodes and
    DataSchema nodes. Arcs between these nodes come in two flavours:
    1. The task schema consumes the data schema.
    2. The task schema produces the data schema.

    The semantics of work flow execution are determined by the TaskGraph.
    """

    def __init__(self, _var_name=_GLOBAL_TASK_GRAPH_VAR):
        self._var_name = _var_name

    def __enter__(self):
        return ScopedGlobal(self._var_name, self).__enter__()

    def __exit__(self, exc_type, exc_value, exc_traceback):
        pass


class Interpreter:
    """Class to interpret a TaskGraph into a structure tailored to a particular
    execution model, e.g., an interpreter that translates a TaskGraph into an
    Airflow DAG, or one that translates it to a command line program.

    Hence, an interpreter needs to be able to interpret an arbitrary TaskSchema
    and an arbitrary DataSchema.

    For example, an interpreter into a command line program would require a
    mapping from task schemas to callables with input and output matching the
    task schema.
    """
    pass


class DataInterpreter:
    pass

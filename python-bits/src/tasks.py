from collection import namedtuple
from abc import ABC, abstractmethod



class TaskDescriptor(ABC):

    Schema = namedtupe('TaskSchema', ['inputs', 'outputs'])

    @property
    def schema(self):
        pass

    @property
    @abstractmethod
    def inputs(self):
        """Returns a frozen dict mapping local names to DataSchema instances."""
        pass

    @property
    def outputs(self):
        """Returns a frozen dict mapping local names to DataSchema instances."""
        pass


class DataDescriptor:

    def schema(self):
        pass


class TaskGraph(TaskSchema):
    """
    Immutable representation of an abstract work flow.  The representation is in
    terms of a directed bipartite graph, consisting of TaskSchema nodes and
    DataSchema nodes. Arcs between these nodes come in two flavours:
    1. The task schema consumes the data schema.
    2. The task schema produces the data schema.

    The semantics of work flow execution are determined by the TaskGraph.
    """

    def connect(self, task_schema, input_schemas):
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

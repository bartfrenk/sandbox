import uuid
from abc import ABC, abstractmethod
from collections import namedtuple

from draft.utils import ScopedGlobal, camel_to_kebab, freeze

_TASK_GRAPH_VAR = '_TASK_GRAPH'


class TaskError(Exception):
    pass


class TaskDescriptor(ABC):
    @property
    @abstractmethod
    def input_descriptors(self):
        pass

    @property
    @abstractmethod
    def output_descriptors(self):
        pass

    def connect(self, data_descriptors):
        graph = globals().get(_TASK_GRAPH_VAR)
        if not graph:
            raise TaskError("No graph in context")
        return graph.add_task(self, data_descriptors)

    @property
    def name(self):
        return camel_to_kebab(self.__class__.__name__)

    def __call__(self, **data_descriptors):
        return self.connect(data_descriptors)


DataDescriptor = namedtuple('DataDescriptor', ['name', 'schema'])


class DataStub:
    def __init__(self, tag, descriptor):
        self.descriptor = descriptor
        self.tag = tag


def from_list(*data_descriptors):
    return {desc.name.replace('-', '_'): desc for desc in data_descriptors}


class TaskGraph(ABC):
    def __init__(self):
        self._global = ScopedGlobal(globals(), _TASK_GRAPH_VAR, self)
        self.data_descriptors = {}
        self.task_descriptors = {}
        self.consumers = {}
        self.producers = {}

    def __enter__(self):
        return self._global.__enter__()

    def __exit__(self, *args, **kwargs):
        return self._global.__exit__(*args, **kwargs)

    def add_task(self, task_descriptor, data_stubs):
        for stub in data_stubs.values():
            if stub.tag not in self.data_descriptors:
                raise TaskError("Unknown data descriptor {}".format(stub.tag))

        task_tag = self._unique_task_tag(task_descriptor)
        self.task_descriptors[task_tag] = task_descriptor

        for (name, desc) in task_descriptor.input_descriptors.items():

            if name in data_stubs:
                if task_tag not in self.consumers:
                    self.consumers[task_tag] = {}
                self.consumers[task_tag][name] = data_stubs[name].tag
            else:
                self.data_descriptors[desc.name] = desc

        output_stubs = self._create_output_stubs(task_tag)
        for stub in output_stubs.values():
            self.data_descriptors[stub.tag] = stub.descriptor
        if output_stubs:
            self.producers[task_tag] = {
                name: stub.tag
                for (name, stub) in output_stubs.items()
            }
        return freeze(**output_stubs)

    def _unique_task_tag(self, task_descriptor):
        while True:
            tag = '{}-{}'.format(task_descriptor.name, str(uuid.uuid4())[:4])
            if tag not in self.task_descriptors:
                return tag

    def _create_output_stubs(self, task_tag):
        task = self.task_descriptors[task_tag]
        stubs = {}
        for desc in task.output_descriptors.values():
            tag = '{}:{}'.format(task_tag, desc.name)
            stubs[desc.name.replace('-', '_')] = DataStub(tag, desc)
        return stubs

import uuid
from abc import ABC, abstractmethod
from utils import freeze, camel_to_kebab

_COMPOSITE_TASK_VAR = '_COMPOSITE_TASK'


class TaskError(Exception):
    pass


class AbstractTask(ABC):
    @property
    @abstractmethod
    def input_schema(self):
        pass

    @property
    @abstractmethod
    def output_schema(self):
        pass

    @property
    @abstractmethod
    def label(self):
        pass

    def connect(self, input_stubs):
        check_stubs(self.input_schema, input_stubs)
        composite = globals().get(_COMPOSITE_TASK_VAR)
        if not composite:
            raise TaskError('No composite task in context')
        return composite.add_task(self, input_stubs)

    def __call__(self, **input_stubs):
        return self.connect(input_stubs)


def _create_output_stubs(producer_tag, producer):
    return freeze(
        **{
            name: Stub.create(name, schema, producer_tag)
            for (name, schema) in producer.output_schema.items()
        })


class Stub:
    def __init__(self, tag, producer_tag, schema):
        self.schema = schema
        self.producer_tag = producer_tag
        self.tag = tag

    @classmethod
    def create(cls, name, schema, producer_tag=None):
        tag = '{}:{}'.format(producer_tag or 'input', name)
        return cls(tag, producer_tag, schema)

    def __repr__(self):
        return '<{}(tag={})>'.format(self.__class__.__name__, self.tag)


class CompositeTask:
    def __init__(self):
        self.tasks = {}
        self.outputs = {}
        self.consumes = []

    def __enter__(self):
        # pylint: disable=attribute-defined-outside-init
        if _COMPOSITE_TASK_VAR in globals():
            self._before = globals()[_COMPOSITE_TASK_VAR]
        globals()[_COMPOSITE_TASK_VAR] = self
        return self

    def __exit__(self, *args, **kwargs):
        if hasattr(self, '_before'):
            globals()[_COMPOSITE_TASK_VAR] = self._before
        else:
            del globals()[_COMPOSITE_TASK_VAR]

    def add_task(self, task, input_stubs):
        for stub in input_stubs.values():
            if stub.tag not in self.outputs:
                raise TaskError('Stub {} unknown'.format(stub))

        task_tag = self._create_task_tag(task)
        self.tasks[task_tag] = task

        for (name, schema) in task.input_schema.items():
            if name in input_stubs:
                self.consumes.append((task_tag, (name, input_stubs[name].tag)))
            else:
                stub = Stub.create(name, schema)
                self.outputs[stub.tag] = stub
                self.consumes.append((task_tag, (name, stub.tag)))
        output_stubs = _create_output_stubs(task_tag, task)
        for stub in output_stubs.values():
            self.outputs[stub.tag] = stub
        return output_stubs

    def _create_task_tag(self, task):
        while True:
            tag = '{}-{}'.format(task.label, str(uuid.uuid4())[:4])
            if tag not in self.tasks:
                return tag


class LabelMixin:
    def __init__(self):
        # IMPROVE: Make this the kebab-cased version of the class name
        self._label = camel_to_kebab(self.__class__.__name__)

    @property
    def label(self):
        return self._label


class AtomicTask(LabelMixin, AbstractTask):
    @property
    @abstractmethod
    def input_schema(self):
        pass

    @property
    @abstractmethod
    def output_schema(self):
        pass

    @abstractmethod
    def run(self, input_data):
        pass


class GetRawData(AtomicTask):

    input_schema = {}
    output_schema = {'landscapes': None, 'performance': None}

    def __init__(self, raw_data_source):
        super().__init__()
        self._raw_data_source = raw_data_source

    def run(self, input_data):
        return {'landscapes': 1, 'performance': 2}


class RemoveOutliers(AtomicTask):

    input_schema = {'performance': None, 'criteria': None}
    output_schema = {'outliers': None, 'features': None}

    def run(self, input_data):
        return {'outliers': [], 'features': [1, 2]}


def test_create_task(raw_data_source=None):
    with CompositeTask() as task:
        raw_data = GetRawData(raw_data_source)()
        RemoveOutliers()(performance=raw_data.performance)

    return task


def check_stubs(schema, stubs):
    pass


task = test_create_task()

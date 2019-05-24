from enum import Enum


class Computation:
    @property
    def input_schema(self):
        pass

    @property
    def output_schema(self):
        pass


class ComputationName(Enum):
    COLLECT_RAW_DATA = ()
    GENERATE_MAPPINGS = ()
    PREPROCESS_DATA = ()
    TRAIN_MODEL = ()


class Data:
    pass


class Schema:
    def __getattr__(self, name):
        pass


class ConstructionError(Exception):
    pass


class AbstractProcess:
    def connect(self, schemas):
        pass

    def __call__(self, **schemas):
        return self.connect(schemas)


class AtomicProcess(AbstractProcess):
    def __init__(self, name, settings=None):
        self.name = name
        self._settings = settings

    def connect(self, schemas):
        composite = globals().get("_COMPOSITE_PROCESS")
        if not composite:
            raise ConstructionError()
        composite.add_process(self)
        for (name, schema) in schemas:
            composite.connect_process(self, name, schema)


class CompositeProcess(AbstractProcess):
    def __init__(self, settings):
        self.settings = settings

    @property
    def context(self):
        return ScopedGlobals(_COMPOSITE_PROCESS=self)


class ScopedGlobals:
    class _Sentinel:
        pass

    def __init__(self, **assignments):
        self._before = {}
        for (name, value) in assignments.items():
            self._before[name] = globals().get(name, self._Sentinel)
            globals()[name] = value

    def __enter__(self):
        pass

    def __exit__(self, *args, **kwargs):
        for (name, value) in self._before.items():
            if value is self._Sentinel:
                del globals()[name]
            else:
                globals()[name] = value


# pylint: disable=undefined-variable


class FromFunction(AbstractProcess):
    pass


def task(settings):
    with CompositeProcess(settings).context as result:
        mappings = GetMappings()()
        performance_data = CollectRawData()()
        preprocessed_data = PreprocessData.connect(
            mappings=mappings, performance_data=performance_data
        )
        model = AtomicProcess(ProcessName.TRAIN_MODEL)(
            mappings=mappings, preprocessed_data=preprocessed_data
        )

    return result


def create_cli_program(task, data_channel):
    return {}


task({})

dag = to_airflow_dag(task)

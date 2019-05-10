from enum import Enum
from tasks import AtomicTask, CompositeTask


class DataStatus(Enum):
    SUCCESS = ()
    FAILURE = ()
    NA = ()


class DataState:
    def __init__(self):
        self.status = DataStatus.NA
        self.data = None


class TaskStatus(Enum):
    SUCCESS = ()
    FAILURE = ()
    PENDING = ()


class DataProxy:
    pass


class TaskState:
    def __init__(self, task):
        self.task = task
        self.status = TaskStatus.PENDING


class Process:
    def __init__(self, task):
        if isinstance(task, CompositeTask):
            self.outputs = {tag: DataState() for tag in task.outputs}
            self.tasks = {tag: TaskState(task) for (tag, task) in task.tasks}

    def run(self, input_data):
        ## Set data input nodes
        for (name, data) in input_data.items():
            tag = 'input:{}'.format(name)
            if tag in self.outputs:
                self.outputs[tag].data = data

        while True:
            tasks = self._get_runnable_tasks()
            if not tasks:
                break
            for (task, arguments) in tasks:
                self._run_task(task, arguments)

    def _get_runnable_tasks(self):
        return []

    def _run_task(self, task, arguments):
        pass

    def _pending(self):
        pass

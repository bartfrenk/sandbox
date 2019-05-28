class SequentialExecutor:
    def __init__(self, task_descriptor, task_mapping):
        self._task_descriptor = task_descriptor
        self._task_mapping = task_mapping

    def run(self, **input_data):
        pass

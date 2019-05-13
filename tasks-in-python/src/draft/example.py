# pylint: disable=unused-wildcard-import, wildcard-import
from draft.descriptors import *


class GetRawData(TaskDescriptor):
    input_descriptors = from_list(DataDescriptor('advertiser-id', {}))
    output_descriptors = from_list(DataDescriptor('bid-landscapes', {}),
                                   DataDescriptor('performance-metrics', {}))


class PrepareData(TaskDescriptor):
    input_descriptors = from_list(DataDescriptor('performance-metrics', {}),
                                  DataDescriptor('advertiser-id', {}))
    output_descriptors = from_list(DataDescriptor('features', {}))


def test():
    with TaskGraph() as composite:
        raw_data = GetRawData()()
        PrepareData()(performance_metrics=raw_data.performance_metrics)
    return composite

# pylint: disable=unused-wildcard-import, wildcard-import
from draft.composite import *
from draft.descriptors import TaskDescriptor

get_raw_data = TaskDescriptor(name='get-raw-data',
                              input_descriptors=[DataDescriptor('advertiser-id', {})],
                              output_descriptors=[
                                  DataDescriptor('bid-landscapes', {}),
                                  DataDescriptor('performance-metrics', {})
                              ])

prepare_data = TaskDescriptor(name='prepare-data',
                              input_descriptors=[
                                  DataDescriptor('performance-metrics', {}),
                                  DataDescriptor('advertiser-id', {})
                              ],
                              output_descriptors=[
                                  DataDescriptor('features', {}),
                              ])


def create_composite():
    with TaskGraph() as composite:
        raw_data = get_raw_data()
        prepare_data(performance_metrics=raw_data.performance_metrics)
    return composite

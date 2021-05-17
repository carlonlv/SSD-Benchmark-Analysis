import os
from os import error, path

import numpy as np
import pandas as pd
from numpy.lib.function_base import copy
from pandas import read_csv
from tqdm import tqdm

head_path = '/mnt/scratch/'

path = head_path + 'google_2019_data/' + 'parsed_task_events/'

target_file_name = 'task_events_df' + '.csv'

parsed_task_events = sorted(os.listdir(path))
#parsed_task_events = ['task_events_df.csv']

if 'task_events_df.csv' in parsed_task_events:
    parsed_task_events.remove('task_events_df.csv')

## Try this if memory is enough
for f in tqdm(parsed_task_events[0:]):
    print(f)
    temp_df = pd.read_csv(
        path + f,
        usecols = ["time", "type", "collection_id", "scheduling_class", "collection_type", "priority", "instance_index", "resource_request.cpus", "resource_request.memory"],
        index_col = 0,
        low_memory = True)

    cols = temp_df.columns
    for j in cols:
        temp_df[j] = pd.to_numeric(temp_df[j], errors = 'coerce')

    temp_df.dropna(inplace = True)
    temp_df.drop_duplicates(inplace = True, ignore_index = True)
    
    if os.path.exists(path + target_file_name):
        temp_df.to_csv(path + target_file_name, mode = 'a', header = False)
    else:
        temp_df.to_csv(path + target_file_name, header = True)

#task_events = task_events.groupby(['collection_id']).size().reset_index(name = 'task_number')
#job_events = pd.read_csv(head_path + 'google_2019_data/parsed_job_events' + '/' + 'job_events_df.csv')
#job_events = job_events.drop_duplicates()
#job_events = job_events.join(task_events, on = 'collection_id', lsuffix='.jobs', rsuffix='.tasks')
#job_events = job_events.drop_duplicates()
#job_events.to_csv(head_path + 'google_2019_data/parsed_job_events' + '/' + 'job_events_df.csv')

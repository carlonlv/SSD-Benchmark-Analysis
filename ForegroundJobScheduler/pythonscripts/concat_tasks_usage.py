import os
from pandas import read_csv
import pandas as pd
from os import path
import numpy as np
import time
import json
import gzip
from tqdm import tqdm
import pickle

head_path = '/mnt/scratch/'

path = head_path + 'google_2019_data/' + 'parsed_task_usage/'

parsed_task_usage = sorted(os.listdir(path))

## Try this if memory is enough
task_usage = []
for f in tqdm(parsed_task_usage[0:]):
    temp_df = pd.read_csv(path + f)
    task_usage.append(temp_df)
task_usage = pd.concat(task_usage, sort = False)
task_usage = task_usage.drop_duplicates()

target_file_name = 'task_usage_df' + '.csv'
task_usage.to_csv(path + target_file_name, header = True)

#task_usage = task_usage.groupby(['collection_id', 'instance_index']).agg({
                #'start_time':lambda y: min([int(i) for i in y]),
                #'end_time': lambda x: max([int(i) for i in x])
            #})
#task_usage['task_duration'] = (task_usage['end_time'] - task_usage['start_time']) / 1e+08

#target_file_name = 'task_usage_df' + '.csv'
#task_usage.to_csv(path + target_file_name, header = True)

#task_usage.drop(columns = ['start_time', 'end_time'])
#task_events = pd.read_csv(head_path + 'google_2019_data/' + 'parsed_task_events/' + 'task_events_df.csv')
#task_events = task_events.join(task_usage, on = ['collection_id', 'instance_index'])
#task_events.to_csv(head_path + 'google_2019_data/' + 'parsed_task_events/' + 'task_events_df.csv')

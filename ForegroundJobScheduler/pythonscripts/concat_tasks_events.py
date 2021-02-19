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

path = head_path + 'google_2019_data/' + 'parsed_task_events/'

parsed_task_events = sorted(os.listdir(path))

## Try this if memory is enough
task_events = []
for f in tqdm(parsed_task_events[0:]):
    temp_df = pd.read_csv(path + f)
    task_events.append(temp_df)
task_events = pd.concat(task_events, sort = False)
task_events = task_events.drop_duplicates()

target_file_name = 'task_events_df' + '.csv'
task_events.to_csv(path + target_file_name, header = True)

#task_events = task_events.groupby(['collection_id']).size().reset_index(name = 'task_number')
#job_events = pd.read_csv(head_path + 'google_2019_data/parsed_job_events' + '/' + 'job_events_df.csv')
#job_events = job_events.drop_duplicates()
#job_events = job_events.join(task_events, on = 'collection_id', lsuffix='.jobs', rsuffix='.tasks')
#job_events = job_events.drop_duplicates()
#job_events.to_csv(head_path + 'google_2019_data/parsed_job_events' + '/' + 'job_events_df.csv')

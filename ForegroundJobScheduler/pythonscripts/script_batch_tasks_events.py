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
import itertools
import multiprocessing as mp

def normalize(line):
    result = pd.json_normalize(json.loads(line), max_level=2)
    #return filter(result)
    if int(result['instance_index']) == 0:
        return result
    else:
        return None 

head_path = '/mnt/scratch/'

path = head_path + 'google_2019_data/'

with open(path + 'selected_job_ids_batch.pkl', 'rb') as r:
    selected_collection_ids = pickle.load(r)

st = time.time()
task_events = sorted(os.listdir(path + 'task_events'))

target_file_name = 'parsed_task_usage/task_events_df' + ',' + str(st) + '.csv'
temp_df = []

for f in tqdm(task_events[0:]):
    r = gzip.open(path + 'task_events' + '/' + f, 'rt')
    r.seek(0, 0)
    r = r.readlines()
    #for line in tqdm(r):
        #temp_df.append(normalize(line))
    with mp.Pool(processes = mp.cpu_count() - 4) as p:
            temp_df.extend(list(tqdm(p.imap(normalize, r), total = len(r))))
    temp_df = pd.concat(temp_df, sort = False)
    temp_df['collection_id'] = temp_df['collection_id'].astype(int)
    temp_df = temp_df[temp_df['collection_id'].isin(selected_collection_ids)]
    if len(temp_df.index) > 0:
        if os.path.exists(path + target_file_name):
            temp_df.to_csv(path + target_file_name, mode = 'a', header = False)
        else:
            temp_df.to_csv(path + target_file_name, header = True)
    temp_df = []
        
et = time.time()
print("Processing task events took" ,et - st ," seconds")

# Apply the conditions
# priority < 119 (free tier <= 99, best effort <= 115, mid-tier <= 119) and
# sched_cls!=3 (latency sensitive tasks) and event_type == 4

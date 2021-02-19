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

def filter(single_line_dat):
    if single_line_dat['collection_id'].isin(selected_collection_ids).to_list()[0]:
        return single_line_dat
    else:
        return None

def normalize(line):
    result = pd.json_normalize(json.loads(line))
    if int(result['instance_index']) == 0:
        return result
    else:
        return None 

head_path = '/mnt/scratch/'

path = head_path + 'google_2019_data/'

with open(path + 'selected_job_ids.txt', 'rb') as r:
    selected_collection_ids = pickle.load(r)

st = time.time()
task_events = sorted(os.listdir(path + 'task_events'))

target_file_name = 'task_events_df' + ',' + str(st) + '.csv'
temp_df = []

for f in tqdm(task_events[0:]):
    r = gzip.open(path + 'task_events' + '/' + f, 'rt')
    r.seek(0, 0)
    r = r.readlines()
    #pool = mp.Pool(processes = 20)
    #mp_result = pool.map(normalize, r)
    #temp_df.extend(mp_result)
    #pool.close()
    #pool.join()
    #temp_df.extend(list(map(normalize, r)))
    for line in tqdm(r):
        temp_df.append(normalize(line))
    if all(v is None for v in temp_df):
        temp_df = []
    else:
        temp_df = pd.concat(temp_df, sort = False)
        temp_df = temp_df[temp_df['collection_id'].isin(selected_collection_ids)]
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

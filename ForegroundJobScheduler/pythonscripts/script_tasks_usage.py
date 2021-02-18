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
task_usage = sorted(os.listdir(path + 'task_usage'))

target_file_name = 'task_usage_df' + ',' + str(st) + '.csv'
temp_df = []
for f in tqdm(task_usage[0:]):
    r = gzip.open(path + 'task_usage' + '/' + f, 'rt')
    r.seek(0, 0)
    r = r.readlines()
    pool = mp.Pool(processes = mp.cpu_count() - 1)
    temp_df.extend(pool.map(normalize, r))
    pool.close()
    pool.join()
    if all(v is None for v in temp_df):
        temp_df = []
    else:
        temp_df = pd.concat(temp_df, sort = False)
        temp_df = temp_df[temp_df['collection_id'].isin(selected_collection_ids)]
        temp_df = temp_df.groupby(['collection_id', 'instance_index']).agg({
            'start_time':lambda y: min([int(i) for i in y]),
            'end_time': lambda x: max([int(i) for i in x])
        })
        if os.path.exists(path + target_file_name):
            temp_df.to_csv(path + target_file_name, mode = 'a', header = False)
        else:
            temp_df.to_csv(path + target_file_name, header = True)

        temp_df = []

et = time.time()
print("Processing task usage took" ,et - st ," seconds")

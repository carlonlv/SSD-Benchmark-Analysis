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
    return result

def process(file_name):
    r = gzip.open(path + 'task_usage' + '/' + file_name, 'rt')
    r.seek(0, 0)
    r = r.readlines()
    temp_df = []
    #pool = mp.Pool(processes = mp.cpu_count() - 1)
    #temp_df.extend(pool.map(normalize, r))
    #pool.close()
    #pool.join()
    with mp.Pool(processes = mp.cpu_count() - 1) as p:
        temp_df.extend(list(tqdm(p.imap(normalize, r), total=len(r))))
    #for line in r:
        #temp_df.append(normalize(line))
    temp_df = pd.concat(temp_df, sort = False)
    temp_df['collection_id'] = temp_df['collection_id'].astype(int)
    temp_df['instance_index'] = temp_df['instance_index'].astype(int)
    temp_df = temp_df[temp_df['collection_id'].isin(selected_collection_ids)]
    if len(temp_df.index) > 0:
        temp_df = temp_df.groupby(['collection_id', 'instance_index']).agg({
            'start_time':lambda y: min([int(i) for i in y]),
            'end_time': lambda x: max([int(i) for i in x])
        })
        if os.path.exists(path + target_file_name):
            temp_df.to_csv(path + 'parsed_task_usage/' + target_file_name,  mode = 'a', header = False)
        else:
            temp_df.to_csv(path + 'parsed_task_usage/' + target_file_name, header = True)


head_path = '/mnt/scratch/'
path = head_path + 'google_2019_data/'

print("Loading batch job ids.")

with open(path + 'selected_job_ids_batch.pkl', 'rb') as r:
    selected_collection_ids = pickle.load(r)

task_usage = sorted(os.listdir(path + 'task_usage'))

st = time.time()
target_file_name = 'batch_task_usage_df' + ',' + str(st) + '.csv'

#with mp.Pool(processes = mp.cpu_count() - 1) as p:
    #tqdm(p.imap(process, task_usage), total=len(task_usage))
for f in tqdm(task_usage[0:]):
    process(f)
et = time.time()
print("Processing task usage took" ,et - st ," seconds")

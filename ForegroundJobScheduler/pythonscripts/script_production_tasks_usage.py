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
    if all(v is None for v in temp_df):
        temp_df = []
        return 0
    else:
        temp_df = pd.concat(temp_df, sort = False)
        temp_df['collection_id'] = temp_df['collection_id'].astype(int)
        temp_df = temp_df[temp_df['collection_id'].isin(selected_collection_ids)]
        unique_collection_ids = set(temp_df['collection_id'])
        print("Found " + str(len(unique_collection_ids)) + " in " + file_name)
        for ids in unique_collection_ids:
            instance_id = temp_df[temp_df['collection_id'] == ids]['instance_index'][0]
            unique_df = temp_df[temp_df['collection_id'] == ids]
            unique_df = unique_df[unique_df['instance_index'] == instance_id]
            unique_df = unique_df.sort_values(by='start_time', ascending = True)
            unique_df.to_csv(path + 'parsed_task_usage/' + 'production_task_usage_df' + ',' + ids + '.csv', header = True)
        return len(unique_collection_ids)


#manager = mp.Manager()
#numer_of_traces = manager.list()
head_path = '/mnt/scratch/'
path = head_path + 'google_2019_data/'

print("Loading production job ids.")

with open(path + 'selected_job_ids_production.pkl', 'rb') as r:
    selected_collection_ids = pickle.load(r)
selected_collection_ids = list(set(selected_collection_ids))
selected_collection_ids = [int(x) for x in selected_collection_ids]
print(len(selected_collection_ids))

task_usage = sorted(os.listdir(path + 'task_usage'))

print("Starting multi-core processing.")

st = time.time()
#with mp.Pool(processes = mp.cpu_count() - 1) as p:
    #tqdm(p.imap(process, task_usage), total=len(task_usage))
for f in tqdm(task_usage[0:]):
    process(f)
et = time.time()
print("Processing task usage took" ,et - st ," seconds")

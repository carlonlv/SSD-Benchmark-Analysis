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

    temp_production_df = temp_df[temp_df['collection_id'].isin(selected__production_collection_ids)]
    temp_batch_df = temp_df[temp_df['collection_id'].isin(selected__batch_collection_ids)]

    if len(temp_production_df.index) > 0:
        unique_collection_ids = set(temp_production_df['collection_id'])
        print("Found " + str(len(unique_collection_ids)) + " in " + file_name)
        for ids in unique_collection_ids:
            instance_id = temp_production_df[temp_production_df['collection_id'] == ids]['instance_index'][0]
            unique_df = temp_production_df[temp_production_df['collection_id'] == ids]
            unique_df = unique_df[unique_df['instance_index'] == instance_id]
            unique_df = unique_df.sort_values(by='start_time', ascending = True)
            unique_df.to_csv(path + 'parsed_task_usage/' + 'production_task_usage_df' + ',' + ids + '.csv', header = True)
        return len(unique_collection_ids)
    
    if len(temp_batch_df.index) > 0:
        temp_batch_df = temp_batch_df.groupby(['collection_id', 'instance_index']).agg({
            'start_time':lambda y: min([int(i) for i in y]),
            'end_time': lambda x: max([int(i) for i in x])
        })
        if os.path.exists(path + target_file_name):
            temp_batch_df.to_csv(path + 'parsed_task_usage/' + target_file_name, mode = 'a', header = False)
        else:
            temp_batch_df.to_csv(path + 'parsed_task_usage/' + target_file_name, header = True)
        return 0
    return 0


#manager = mp.Manager()
#numer_of_traces = manager.list()
head_path = '/mnt/scratch/'
path = head_path + 'google_2019_data/'

with open(path + 'selected_job_ids_production.pkl', 'rb') as r:
    selected__production_collection_ids = pickle.load(r)
with open(path + 'selected_job_ids_batch.pkl', 'rb') as r:
    selected__batch_collection_ids = pickle.load(r)

task_usage = sorted(os.listdir(path + 'task_usage'))

print("Starting multi-core processing.")

st = time.time()
target_file_name = 'batch_task_usage_df' + ',' + str(st) + '.csv'
#with mp.Pool(processes = mp.cpu_count() - 1) as p:
    #tqdm(p.imap(process, task_usage), total=len(task_usage))
for f in tqdm(task_usage[0:]):
    process(f)
et = time.time()
print("Processing task usage took" ,et - st ," seconds")

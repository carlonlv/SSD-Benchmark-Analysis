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
import re

def normalize(line):
    result = pd.json_normalize(json.loads(line), max_level=2)
    return result

def process(file_name):
    temp_df = []

    r = gzip.open(path + 'task_usage' + '/' + file_name, 'rt')
    r.seek(0, 0)
    r = r.readlines()
    with mp.Pool(processes = mp.cpu_count()) as p:
        temp_df.extend(list(tqdm(p.imap(normalize, r, chunksize = 3000), total=len(r))))
    del r[:]
    del r

    manager = mp.Manager()
    temp_df = manager.list(temp_df)

    indices = np.arange(0, len(temp_df), len(temp_df) // (32 * mp.cpu_count()))
    indices.append(len(temp_df))

    def filter(index):
        tt_df = pd.concat(temp_df[indices[index]:indices[index + 1]], sort = False)
        temp_df[indices[index]:indices[index + 1]] = [None for _ in range(indices[index], indices[index + 1])]
        tt_df['collection_id'] = tt_df['collection_id'].astype(int)
        tt_batch_indicator = tt_df['collection_id'].isin(selected__batch_collection_ids)
        tt_batch_df = tt_df[tt_batch_indicator]
        tt_df = tt_df[~tt_batch_df]
        tt_production_df = tt_df[tt_df['collection_id'].isin(selected__production_collection_ids)]
        return [tt_production_df, tt_batch_df]

    ab_temp_df = []
    with mp.Pool(processes = mp.cpu_count()) as p:
         ab_temp_df.extend(list(tqdm(p.imap(filter, range(0, len(indices) - 1), chunksize = 2), total=len(indices) - 1)))

    temp_batch_df = pd.DataFrame()
    temp_production_df = pd.DataFrame()
    for x in tqdm(ab_temp_df):
        temp_batch_df = temp_batch_df.append(x[1], ignore_index = True)
        temp_production_df = temp_production_df.append(x[0], ignore_index = True)
    
    val = 0

    if len(temp_batch_df.index) > 0:
        temp_batch_df = temp_batch_df.groupby(['collection_id', 'instance_index']).agg({
            'start_time':lambda y: min([int(i) for i in y]),
            'end_time': lambda x: max([int(i) for i in x])
        })
        if os.path.exists(path + target_file_name):
            temp_batch_df.to_csv(path + 'parsed_task_usage/' + target_file_name, mode = 'a', header = False)
        else:
            temp_batch_df.to_csv(path + 'parsed_task_usage/' + target_file_name, header = True)
            
    if len(temp_production_df.index) > 0 and current_production_count < max_production_count:
        unique_collection_ids = set(temp_production_df['collection_id'])
        for ids in unique_collection_ids:
            instance_id = temp_production_df[temp_production_df['collection_id'] == ids]['instance_index'][0]
            unique_df = temp_production_df[temp_production_df['collection_id'] == ids]
            unique_df = unique_df[unique_df['instance_index'] == instance_id]
            unique_df = unique_df.sort_values(by='start_time', ascending = True)
            if len(unique_df.index) >= 3000:
                unique_df.to_csv(path + 'parsed_task_usage/' + 'production_task_usage_df' + ',' + str(ids) + '.csv', header = True)
                val += 1
        print("Found " + str(val) + " in " + file_name)

    temp_production_df = []
    temp_batch_df = []

    os.remove(path + 'task_usage' + '/' + file_name)
    return val


#manager = mp.Manager()
#numer_of_traces = manager.list()
head_path = '/mnt/scratch/'
path = head_path + 'google_2019_data/'

with open(path + 'selected_job_ids_production.pkl', 'rb') as r:
    selected__production_collection_ids = pickle.load(r)
#selected__production_collection_ids = pd.Series(selected__production_collection_ids, name = 'collection_id', dtype = 'int')
with open(path + 'selected_job_ids_batch.pkl', 'rb') as r:
    selected__batch_collection_ids = pickle.load(r)
#selected__batch_collection_ids = pd.Series(selected__batch_collection_ids, name = 'collection_id', dtype = 'int')

processed_tasks = sorted(os.listdir(path + 'parsed_task_usage'))
processed_production_tasks = [x for x in processed_tasks if re.match("production_task_usage_df,*", x)]
print("Already processed " +  str(len(processed_production_tasks)) + " production tasks.")

task_usage = sorted(os.listdir(path + 'task_usage'))

max_production_count = 3000 - len(processed_production_tasks)
current_production_count = 0

st = time.time()
target_file_name = 'batch_task_usage_df' + ',' + str(st) + '.csv'
#with mp.Pool(processes = mp.cpu_count() - 1) as p:
    #tqdm(p.imap(process, task_usage), total=len(task_usage))
for f in tqdm(task_usage[0:]):
    current_production_count += process(f)
et = time.time()
print("Processing task usage took" ,et - st ," seconds")

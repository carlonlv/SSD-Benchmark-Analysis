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
import functools
from tqdm.contrib.concurrent import process_map
import gc


def normalize(line):
    result = pd.json_normalize(json.loads(line), max_level=2)
    return result

def process(file_name):
    r = gzip.open(path + 'task_usage' + '/' + file_name, 'rt')
    r.seek(0, 0)
    r = r.readlines()

    #process_map(functools.partial(normalize_and_filter, selected__batch_collection_ids, selected__production_collection_ids), r, max_workers = mp.cpu_count(), chunksize = 600)
    temp_df = process_map(normalize, r, max_workers = mp.cpu_count(), chunksize = 400)

    del r
    gc.collect()
    print("Memory drop from r")
    
    temp_df = pd.concat(temp_df, sort = False)
    #column_names = temp_df[0].columns
    #df_dict = dict.fromkeys(column_names, [])
    #for col in tqdm(column_names):
        #extracted = [frame[col] for frame in temp_df]
        #df_dict[col] = extracted
    #temp_df = pd.DataFrame.from_dict(df_dict)[column_names]
    #dict.clear(df_dict)
    temp_df['collection_id'] = temp_df['collection_id'].astype(int, copy = False)
    

    temp_batch_df = pd.merge(selected__batch_collection_ids, temp_df, how = 'inner', on = 'collection_id')
    temp_production_df = pd.merge(selected__production_collection_ids, temp_df, how = 'inner', on = 'collection_id')
    
    del temp_df
    gc.collect()
    print("Memory drop from temp_df")
    
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

    del temp_batch_df
    del temp_production_df
    gc.collect()
    print("Memory drop from temp_batch_df, temp_batch_df")

    os.remove(path + 'task_usage' + '/' + file_name)
    return val


#manager = mp.Manager()
#numer_of_traces = manager.list()
head_path = '/mnt/scratch/'
path = head_path + 'google_2019_data/'

with open(path + 'selected_job_ids_production.pkl', 'rb') as r:
    selected__production_collection_ids = pickle.load(r)
selected__production_collection_ids = pd.Series(selected__production_collection_ids, name = 'collection_id', dtype = 'int')
with open(path + 'selected_job_ids_batch.pkl', 'rb') as r:
    selected__batch_collection_ids = pickle.load(r)
selected__batch_collection_ids = pd.Series(selected__batch_collection_ids, name = 'collection_id', dtype = 'int')

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
    print(f)
    current_production_count += process(f)
et = time.time()
print("Processing task usage took" ,et - st ," seconds")

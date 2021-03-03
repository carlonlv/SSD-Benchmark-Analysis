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

total_count = 0

def filter(single_line_dat):
    if (int(single_line_dat['collection_id']) in collections_found) :
        return None
    elif (int(single_line_dat['collection_id']) in selected_collection_ids):
        return single_line_dat
    else:
        return None

def normalize(line):
    result = pd.json_normalize(json.loads(line), max_level=1)
    return filter(result)

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
    #for line in tqdm(r):
        #temp_df.append(normalize(line))
    if all(v is None for v in temp_df):
        temp_df = []
        return 0
    else:
        temp_df = pd.concat(temp_df, sort = False)
        unique_collection_ids = set(temp_df['collection_id'])
        print("Found " + str(len(unique_collection_ids)) + " in " + file_name)
        for ids in unique_collection_ids:
            collections_found.append(int(ids))
            instance_id = temp_df[temp_df['collection_id'] == ids]['instance_index'][0]
            unique_df = temp_df[temp_df['collection_id'] == ids]
            unique_df = unique_df[unique_df['instance_index'] == instance_id]
            unique_df = unique_df.sort_values(by='start_time', ascending = True)
            unique_df.to_csv(path + 'processed_task_usage/' + 'task_usage_df' + ',' + ids + '.csv', header = True)
        return len(unique_collection_ids)


#manager = mp.Manager()
#numer_of_traces = manager.list()
head_path = '/mnt/scratch/'
path = head_path + 'google_2019_data/'

print("Loading high priority ids.")

with open(path + 'selected_job_ids_high_prior.pkl', 'rb') as r:
    selected_collection_ids = pickle.load(r)

task_usage = sorted(os.listdir(path + 'task_usage'))

print("Starting multi-core processing.")

st = time.time()
#with mp.Pool(processes = mp.cpu_count() - 1) as p:
    #total_count = sum(list(tqdm(p.imap(process, task_usage[0:]),
    #total=len(task_usage[0:]))))
collections_found = [1298, 1925, 200068, 200071, 200492, 200525, 200531, 201425, 201849, 201853, 201857, 201978, 2806, 2947, 3017080, 3159, 32238, 32339, 32345, 32442, 33143, 33240, 359, 47833, 48031, 48708, 48710, 48712, 49340, 49800, 49802, 50887, 530, 58392, 7531, 7536, 786858, 786925]
for f in tqdm(task_usage[0:]):
    if total_count >= 10000:
        break
    else:
        print(collections_found)
        print("Current is " + str(total_count))
        total_count += process(f)
et = time.time()
print("Total count is:" + str(total_count))
print("Processing task usage took" ,et - st ," seconds")

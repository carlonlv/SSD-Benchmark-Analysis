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
    if single_line_dat['collection_id'].isin(selected_collection_ids).to_list()[0] and int(single_line_dat['instance_index']) == 0:
        return single_line_dat
    else:
        return None

def normalize(line):
    result = pd.json_normalize(json.loads(line))
    return filter(result)

def process(file_name, path, enough_sampled):
    if not enough_sampled:
        r = gzip.open(path + 'task_usage' + '/' + file_name, 'rt')
        r.seek(0, 0)
        r = r.readlines()
        temp_df = []
        #pool = mp.Pool(processes = mp.cpu_count() - 1)
        #temp_df.extend(pool.map(normalize, r))
        #pool.close()
        #pool.join()
        #with mp.Pool(processes = mp.cpu_count() - 1) as p:
            #temp_df.extend(list(tqdm(p.imap(normalize, r), total=len(r))))
        for line in tqdm(r):
            temp_df.append(normalize(line))
        if all(v is None for v in temp_df):
            temp_df = []
            numer_of_traces.append(0)
            return 0
        else:
            temp_df = pd.concat(temp_df, sort = False)
            unique_collection_ids = set(temp_df['collection_id'])
            for ids in unique_collection_ids:
                unique_df = temp_df[temp_df['collection_id'] == ids]
                unique_df.to_csv(path + 'task_usage_df' + ',' + ids + '.csv', header = True)
            numer_of_traces.append(len(unique_collection_ids))
            return len(unique_collection_ids)
    else:
        numer_of_traces.append(0)
        return 0


manager = mp.Manager()
numer_of_traces = manager.list()
head_path = '/home/carlonlv/Documents/Research-Projects/ForegroundJobScheduler/pythonscripts/'
path = head_path + 'google_2019_data/'

with open(path + 'selected_job_ids_high_prior.pkl', 'rb') as r:
    selected_collection_ids = pickle.load(r)

task_usage = sorted(os.listdir(path + 'task_usage'))
st = time.time()
with mp.Pool(processes = mp.cpu_count() - 1) as p:
    total_count = sum(list(tqdm(p.imap(normalize, r), total=len(r))))
#for f in tqdm(task_usage[0:]):
    #total_count += process(f, path, total_count >= 10000)
et = time.time()
print("Total count is:" + str(total_count))
print("Processing task usage took" ,et - st ," seconds")

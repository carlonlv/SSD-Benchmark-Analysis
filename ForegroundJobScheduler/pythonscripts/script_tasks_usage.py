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
from pympler import muppy, summary
import objgraph


def normalize(line):
    result = pd.json_normalize(json.loads(line), max_level=2)
    return result

def process(file_name):
    r = gzip.open(path + 'task_usage' + '/' + file_name, 'rt')
    r.seek(0, 0)
    r = r.readlines()

    temp_df = [json.loads(x) for x in r]
    temp_df = pd.json_normalize(temp_df, max_level=2)

    del r
    
    temp_df['collection_id'] = temp_df['collection_id'].astype(int)
    temp_batch_df = temp_df[temp_df['collection_id'].isin(selected__batch_collection_ids)].reset_index()
    
    del temp_df

    val = 0

    if len(temp_batch_df.index) > 0:
        temp_batch_df[['collection_id', 'instance_index']] = temp_batch_df[['collection_id', 'instance_index']].astype(int)
        temp_batch_df = temp_batch_df.groupby(['collection_id', 'instance_index'])
        stime = temp_batch_df['start_time'].min().reset_index()
        etime = temp_batch_df['end_time'].max().reset_index()
        temp_batch_df = pd.merge(stime, etime, how = 'inner')

        if os.path.exists(path + target_file_name):
            temp_batch_df.to_csv(path + 'parsed_task_usage/' + target_file_name, mode = 'a', header = False)
        else:
            temp_batch_df.to_csv(path + 'parsed_task_usage/' + target_file_name, header = True)

    os.remove(path + 'task_usage' + '/' + file_name)
    return val


if __name__ == "__main__":
    head_path = '/mnt/scratch/'
    path = head_path + 'google_2019_data/'

    with open(path + 'selected_job_ids_batch.pkl', 'rb') as r:
        selected__batch_collection_ids = pickle.load(r)
    #selected__batch_collection_ids = pd.Series(selected__batch_collection_ids, name = 'collection_id', dtype = 'int')

    task_usage = sorted(os.listdir(path + 'task_usage'))

    st = time.time()
    target_file_name = 'batch_task_usage_df' + ',' + str(st) + '.csv'
    process_map(process, task_usage, max_workers = 5, chunksize = 1)
    et = time.time()
    print("Processing task usage took" ,et - st ," seconds")

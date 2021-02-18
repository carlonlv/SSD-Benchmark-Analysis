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
import multiprocessing as mp

def filter(dat):
    ## You can add more constraints here.
    if int(dat['priority']) >= 120:
        return True
    else:
        return False

def normalize_and_filter(line):
    result = pd.json_normalize(json.loads(line), max_level=1)
    if filter(result):
        return result['collection_id']
    else:
        return None

head_path = '/mnt/scratch/'

path = head_path + 'google_2019_data/'

st = time.time()
job_events = sorted(os.listdir(path + 'job_events'))

target_file_name = 'selected_job_ids_high_prior' + ',' + str(st) + '.pkl'

selected_job_ids = []
for f in tqdm(job_events[0:]):
    r = gzip.open(path + 'job_events' + '/' + f, 'rt')
    r.seek(0, 0)
    r = r.readlines()
    #pool = mp.Pool(processes = mp.cpu_count() - 1)
    #mp_result = pool.map(normalize_and_filter, r)
    mp_result = []
    for i in tqdm(r):
        mp_result.append(normalize_and_filter(i))
    selected_job_ids.extend(mp_result)
    #pool.close()
    #pool.join()
    selected_job_ids = [x for x in selected_job_ids if x is not None]

et = time.time()
print("Processing task events took" ,et - st ," seconds")
print(len(selected_job_ids))
selected_job_ids = pd.DataFrame(selected_job_ids, columns = ['job_id'])
selected_job_ids.to_pickle(path + target_file_name)
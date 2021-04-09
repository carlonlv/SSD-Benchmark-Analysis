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
    if int(dat['priority']) <= 119:
        return True
    else:
        return False

def normalize_and_filter(line):
    result = pd.json_normalize(json.loads(line), max_level=2)
    if filter(result):
        return int(result['collection_id'])
    else:
        return None

head_path = '/mnt/scratch/'

path = head_path + 'google_2019_data/'

st = time.time()
job_events = sorted(os.listdir(path + 'job_events'))

target_file_name = 'selected_job_ids_low_priority' + ',' + str(st) + '.pkl'

selected_job_ids = []
for f in tqdm(job_events[0:]):
    r = gzip.open(path + 'job_events' + '/' + f, 'rt')
    r.seek(0, 0)
    r = r.readlines()
    with mp.Pool(processes = mp.cpu_count() - 1) as p:
        selected_job_ids.extend(list(tqdm(p.imap(normalize_and_filter, r), total=len(r)))) 
    #for line in tqdm(r):
        #tmp = normalize_and_filter(line)
        #if tmp is not None:
            #selected_job_ids.append(tmp)
print(len(selected_job_ids))

et = time.time()
print("Processing task events took" ,et - st ," seconds")

selected_collection_ids = list(set(selected_job_ids))
selected_collection_ids = [int(x) for x in selected_collection_ids if x != None]
with open(path + target_file_name, 'wb') as r:
    pickle.dump(selected_collection_ids, r, protocol = pickle.HIGHEST_PROTOCOL)

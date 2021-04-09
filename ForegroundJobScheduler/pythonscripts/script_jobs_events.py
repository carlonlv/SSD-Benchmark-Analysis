import gzip
import json
import multiprocessing as mp
import os
import pickle
import time
from os import path

import numpy as np
import pandas as pd
from pandas import read_csv
from tqdm import tqdm

def normalize(line):
    result = pd.json_normalize(json.loads(line), max_level=2)
    return result 

head_path = '/mnt/scratch/'

path = head_path + 'google_2019_data/'

st = time.time()
job_events = sorted(os.listdir(path + 'job_events'))

target_file_name = 'job_events_df' + ',' + str(st) + '.csv'
temp_df = []

for f in tqdm(job_events[0:]):
    r = gzip.open(path + 'job_events' + '/' + f, 'rt')
    r.seek(0, 0)
    r = r.readlines()
    #pool = mp.Pool(processes = mp.cpu_count() - 1)
    #mp_result = pool.map(normalize, r)
    #temp_df.extend(mp_result)
    #pool.close()
    #pool.join()
    with mp.Pool(processes = mp.cpu_count() - 1) as p:
            temp_df.extend(list(tqdm(p.imap(normalize, r), total = len(r))))

    if all(v is None for v in temp_df):
        temp_df = []
    else:
        temp_df = pd.concat(temp_df, sort = False)
        if os.path.exists(path + target_file_name):
            temp_df.to_csv(path + target_file_name, mode = 'a', header = False)
        else:
            temp_df.to_csv(path + target_file_name, header = True)
        temp_df = []
        
et = time.time()
print("Processing task events took" ,et - st ," seconds")

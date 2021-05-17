import os
import pandas as pd
from os import path
from tqdm import tqdm
import re

head_path = '/mnt/scratch/'

path = head_path + 'google_2019_data/' + 'parsed_task_usage/'

parsed_task_usage = sorted(os.listdir(path))

target_file_name = 'task_usage_df' + '.csv'

batch_file_re = re.compile("^batch_task_usage_df,.")

parsed_task_usage = list(filter(batch_file_re.match, parsed_task_usage))

## Try this if memory is enough
task_usage = []
for f in tqdm(parsed_task_usage):
    temp_df = pd.read_csv(path + f, index_col = 0)
    temp_df.drop_duplicates(inplace = True, ignore_index = True)
    temp_df.dropna(inplace = True)

    task_usage.append(temp_df)

    if os.path.exists(path + target_file_name):
        temp_df.to_csv(path + target_file_name, mode = 'a', header = False)
    else:
        temp_df.to_csv(path + target_file_name, header = True)

task_usage = pd.concat(task_usage, sort = False)
task_usage = task_usage.drop_duplicates()

task_usage.to_csv(path + target_file_name, header = True)

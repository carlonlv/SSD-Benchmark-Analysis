import pandas as pd
import numpy as np
import statistics


def check_constraints(data_frame):
    if len(data_frame) < 8032 or (
            statistics.mean(data_frame['max_cpu']) <= 10 and statistics.mean(data_frame['avg_cpu']) <= 8):
        return False
    else:
        return True


if __name__ == '__main__':

    data_path = "C:\\Users\\carlo\\Documents\\GitHub\\Research-Projects\\ForegroundJobScheduler\\pythonscripts\\"
    input_fg_job_name = "list of foreground jobs.csv"
    input_bg_job_name = "list of background jobs.csv"
    number_of_fg_job_sampled = 0
    number_of_bg_job_sampled = 1000

    #fg_jobs_pool = pd.read_csv(data_path + input_fg_job_name)['0'].tolist()
    bg_jobs_pool = pd.read_csv(data_path + input_bg_job_name)['0'].tolist()

    bg_jobs_pool_satisfying_constraints = []
    read_file_data_path = "C:\\Users\\carlo\\Documents\\datasets\\\originaldata\\"
    for filename in bg_jobs_pool:
        print(filename)
        data_frame = pd.read_pickle(read_file_data_path + filename, compression=None)
        satisfy_constraints = check_constraints(data_frame)
        print(satisfy_constraints)
        if satisfy_constraints:
            bg_jobs_pool_satisfying_constraints.append(filename)

    #sampled_foreground = pd.DataFrame(np.random.choice(fg_jobs_pool, number_of_fg_job_sampled, replace=False))
    #sampled_foreground.to_csv("list of sampled foreground jobs.csv", index=False)
    sampled_background = pd.DataFrame(np.random.choice(bg_jobs_pool_satisfying_constraints, number_of_bg_job_sampled, replace=False))
    sampled_background.to_csv("list of sampled background jobs.csv", index=False)

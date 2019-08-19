import pandas as pd
import numpy as np

if __name__ == '__main__':

    data_path = "C:\\Users\\carlo\\Documents\\GitHub\\Research-Projects\\ForegroundJobScheduler\\pythonscripts\\"
    input_bg_job_name = "list of sampled background jobs.csv"
    number_of_bg_job_sampled = 100
    bg_jobs_pool = pd.read_csv(data_path + input_bg_job_name)['0'].tolist()

    sampled_background = pd.DataFrame(
        np.random.choice(bg_jobs_pool, number_of_bg_job_sampled, replace=False))
    sampled_background.to_csv("list of sampled " + str(number_of_bg_job_sampled) + " background jobs.csv", index=False)

import pandas as pd

if __name__ == '__main__':

    input_data_path = "C:\\Users\\carlo\\Documents\\GitHub\\Research-Projects\\ForegroundJobScheduler\\pythonscripts\\"
    output_data_path_bg = "C:\\Users\\carlo\\Documents\\datasets\\csvalldata\\sample background jobs\\"
    output_data_path_fg = "C:\\Users\\carlo\\Documents\\datasets\\csvalldata\\foreground_jobs\\"
    read_data_path = "C:\\Users\\carlo\\Documents\\datasets\\\originaldata\\"

    background_jobs = pd.read_csv(input_data_path + "list of sampled background jobs.csv")['0'].tolist()
    foreground_jobs = pd.read_csv(input_data_path + "list of foreground jobs.csv")['0'].tolist()

    for filenames in background_jobs:
        print("background jobs " + filenames)
        job_num = filenames.split('.')[0]
        read_file = pd.read_pickle(read_data_path + filenames, compression=None)
        read_file.to_csv(path_or_buf=output_data_path_bg + job_num + ".csv")

    #for filenames in foreground_jobs:
        #print("foreground jobs " + filenames)
        #job_num = filenames.split('.')[0]
        #read_file = pd.read_pickle(read_data_path + filenames, compression=None)
        #read_file.to_csv(path_or_buf=output_data_path_fg + job_num + ".csv")

    print('done')


import pandas as pd
import os
import numpy as np

if __name__ == '__main__':

    data_path = "C:\\Users\\carlo\\Documents\\datasets\\originaldata\\"
    arr = os.listdir(data_path)

    background_jobs = []
    foreground_jobs = []

    error_filenames = []

    for filenames in arr:
        print(filenames)
        trace_dataframe = pd.read_pickle(data_path + filenames, compression=None)

        try:
            if 'timestamp' in trace_dataframe.columns:
                runtimelst = trace_dataframe['timestamp']
            else:
                runtimelst = trace_dataframe.axes[0].tolist()

            if round((max(runtimelst) - min(runtimelst)) / 3600) >= 700:
                background_jobs.append(filenames)
            elif round((max(runtimelst) - min(runtimelst)) / 3600) <= 50:
                foreground_jobs.append(filenames)

        except TypeError:
            error_filenames.append(filenames)

    background = pd.DataFrame(background_jobs)
    background.to_csv("list of background jobs.csv", index=False)
    foreground = pd.DataFrame(foreground_jobs)
    foreground.to_csv("list of foreground jobs.csv", index=False)
    error = pd.DataFrame(error_filenames)
    error.to_csv("list of error files.csv", index=False)
    print("Done!")


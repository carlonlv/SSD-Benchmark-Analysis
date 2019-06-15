import pandas as pd

if __name__ == '__main__':
    file_list = [1227464, 1926479]
    for num in file_list:
        trace_dataframe = pd.read_pickle("C:\\Users\\carlo\\PycharmProjects\\ForegroundJobScheduler\\datasets\\alldata\\" + str(num) + ".pd", compression=None)
        trace_dataframe.to_csv(path_or_buf="C:\\Users\\carlo\\PycharmProjects\\ForegroundJobScheduler\\datasets\\csvalldata\\" + str(num) + ".csv")

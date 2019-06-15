import numpy as np
import pandas as pd
import urllib.request
import gzip
import os

if __name__ == '__main__':
    data_path = "C:\\Users\\carlo\\PycharmProjects\\ForegroundJobScheduler\\datasets\\vmtable.csv"
    headers=['vmid','subscriptionid','deploymentid','vmcreated', 'vmdeleted', 'maxcpu', 'avgcpu', 'p95maxcpu', 'vmcategory', 'vmcorecount', 'vmmemory']
    trace_dataframe = pd.read_csv(data_path, header=None, index_col=False,names=headers,delimiter=',')

    deployment_data_path = "C:\\Users\\carlo\\PycharmProjects\\ForegroundJobScheduler\\datasets\\deployment.csv"
    deployment_headers=['deploymentid','deploymentsize']
    deployment_trace_dataframe = pd.read_csv(deployment_data_path, header=None, index_col=False,names=deployment_headers,delimiter=',')

    # Compute VM Lifetime based on VM Created and VM Deleted timestamps and transform to Hour
    trace_dataframe['lifetime'] = np.maximum((trace_dataframe['vmdeleted'] - trace_dataframe['vmcreated']),300)/ 3600

    largestlifetime = trace_dataframe.loc[trace_dataframe['lifetime'].idxmax()]
    longestvmid = largestlifetime['vmid']

    cpuinfoheader = ['timestamp', 'vmid', 'mincpu', 'maxcpu', 'avgcpu']
    cpureadingfile1 = pd.read_csv("C:\\Users\\carlo\\PycharmProjects\\ForegroundJobScheduler\\datasets\\vm_cpu_readings-file-1-of-125.csv", header = None, index_col = False,names = cpuinfoheader, delimiter = ',')
    longestvminfo = pd.DataFrame(columns=cpuinfoheader)
    longestvminfo.to_csv(path_or_buf="C:\\Users\\carlo\\PycharmProjects\\ForegroundJobScheduler\\datasets\\longestvminfotest.csv")

    f = open("url.txt","r")
    urllist = [line.rstrip('\n') for line in f]
    f.close()

    for i in range(12, 136):
        print(i)
        downloadfilename = "C:\\Users\\carlo\\PycharmProjects\\ForegroundJobScheduler\\datasets\\cpufile" + str(i) +".csv.gz"
        print("Retrieving files.")
        urllib.request.urlretrieve(urllist[i], downloadfilename)
        f = gzip.open(downloadfilename)
        print("Reading csv files.")
        cpufile = pd.read_csv(f, names = cpuinfoheader)
        f.close()
        print("Extracting information.")
        a = pd.DataFrame(cpufile[cpufile['vmid'] == longestvmid], columns=cpuinfoheader)
        a.to_csv(
            path_or_buf="C:\\Users\\carlo\\PycharmProjects\\ForegroundJobScheduler\\datasets\\longestvminfotest.csv", mode='a', header=False)
        print("Removing file.")
        os.remove(downloadfilename)
    print("Converting to csv.")

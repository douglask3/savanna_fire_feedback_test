import iris
import iris.coord_categorisation
import netCDF4

import matplotlib.pyplot as plt
import iris.plot as iplt
import numpy as np
import os
from shutil import copyfile

from pdb import set_trace as browser
import iris.quickplot as qplt

dir_in   = "/data/dynamic/dkelley/CRU-JRA-n96e/pre/"
dir_outs = ["/data/dynamic/dkelley/CRU-JRA-n96e_rainfallDist/",
           "/data/dynamic/dkelley/CRU-JRA-n96e_HighRainfall/"]

files = os.listdir(dir_in)

def rainfall_distribution(dat):
    dat_man = dat[0].copy()
    iris.coord_categorisation.add_day_of_year(dat_man, 'time')
    
    dcycle = dat_man[0:4].copy()
    for day in range(1, 365): dcycle.data += dat_man[(day*4):((day+1)*4)].data

    dcycle /= 365  
    daily = dat_man.aggregated_by(['day_of_year'], iris.analysis.MEAN)
    MAP = dat_man.collapsed('time', iris.analysis.MEAN)

    ## put mean cycle on daily rainfalls of zero
    for day in range(0, 365):
        scale = MAP / daily[day]

        test1 = daily[day].data == 0
        test2 = np.logical_not(test1)
        for time in range(0, 4):
            timeD = day*4 + time 
            dat[0].data[timeD][test1] = dcycle[time].data[test1]
            dat[0].data[timeD][test2] *= scale.data[test2]
    browser()
    return dat

def high_rainfall(dat):
    dat0 = dat[0].copy()
    MAP = dat[0].collapsed('time', iris.analysis.MEAN)
    scale = MAP.copy()

    test1 = MAP.data > 0.0001
    test2 = np.logical_not(test1)
    scale.data[test1] = 1.0
    scale.data[test2] = 0.0001/MAP.data[test2]
    
    test1 = MAP.data == 0
    test2 = np.logical_not(test1)
    for time in range(0, dat[0].shape[0]):
        dat[0].data[time][test1] = 0.0001
        dat[0].data[time][test2] = dat[0].data[time][test2] * scale.data[test2]
    
    return dat   
    

def openAndMakeExperimentFile(file, dir_out, FUN):
    print("file in" + dir_in + file)
    copyfile(dir_in + file, dir_out + file)
    file_out = dir_out + file 

    dat = iris.load(file_out)
    dat = FUN(dat)
    
    print("file out" + file_out)
    dset = netCDF4.Dataset(file_out, 'r+')
    dset.variables['pre'][:] = dat[0].data
    dset.close()
    
        
def openAndMakeExperiment(*args, **kw):
    [openAndMakeExperimentFile(file, *args, **kw) for file in files]

openAndMakeExperiment(dir_outs[0], rainfall_distribution)
openAndMakeExperiment(dir_outs[1], high_rainfall)

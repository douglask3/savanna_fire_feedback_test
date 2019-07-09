import iris
import iris.coord_categorisation
import matplotlib.pyplot as plt
import iris.plot as iplt
import numpy as np
import os
from shutil import copyfile

from pdb import set_trace as browser
import iris.quickplot as qplt

dir_in  = "/data/dynamic/dkelley/CRU-JRA-n96e/pre/"
dir_out = "/data/dynamic/dkelley/CRU-JRA-n96e_rainfallDist/"

files = os.listdir(dir_in)

def openAndRedistribute(file):
    copyfile(dir_in + file, dir_out + file)
    dat = iris.load(dir_out + file)
    iris.coord_categorisation.add_day_of_year(dat[0], 'time')
    
    dcycle = dat[0][0:4].copy()
    for day in range(1, 365): dcycle.data += dat[0][(day*4):((day+1)*4)].data

    dcycle /= 365  
    daily = dat[0].aggregated_by(['day_of_year'], iris.analysis.MEAN)
    MAP = dat[0].collapsed('time', iris.analysis.MEAN)

    ## put mean cycle on daily rainfalls of zero
    for day in range(0, 365):
        scale = MAP / daily[day]

        test1 = daily[day].data == 0
        test2 = np.logical_not(test1)
        for time in range(0, 4):
            timeD = day*4 + time 
            dat[0][timeD].data[test1] = dcycle[time].data[test1]
            dat[0][timeD].data[test2] *= scale.data[test2]
    
    file_out = dir_out + file    
    browser()

    
    
    
    

dat = [openAndRedistribute(file) for file in files]

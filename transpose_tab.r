#source("cfg.r")
file = 'outputs/which_exp_is_biggest_comb.csv'

dat0 = read.csv(file)
rownames(dat0) = dat0[,1]
dat0 = dat0[,-1]
datm = dat0

nr = nrow(dat0)

for (i in 1:(nr/2)) for (j in 1:nr) {
    
    ii = i * 2 - c(1, 0)
    jj = j * 2 - c(1, 0)
    if (is.na(dat0[jj[1], i])) next
    if (i>j) {        
        dat0[jj[1], i] = 100 - dat0[jj[1], i]
        dat0[ii[1], j] = 100 - dat0[jj[1], i]
        dat0[ii[2], j] = dat0[jj[2], i]
    } else if (j>i) {
        datm[ii[1], j] = 100 - datm[ii[1], j]
        datm[jj[1], i] = 100 - datm[ii[1], j]
        datm[jj[2], i] = datm[ii[2], j]
    }
    
}

write.csv(dat0, 'outputs/which_exp_is_biggest_TabS2.csv')
write.csv(datm, 'outputs/which_exp_is_biggest_TabS3.csv')

    
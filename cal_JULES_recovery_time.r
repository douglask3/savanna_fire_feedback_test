######################
## cfg				##
######################
source("cfg.r")
graphics.off()

dat = loadInputData()
params = read.csv(paramFile, stringsAsFactors=FALSE)

temp_file = 'temp/cal_jules_lambda_data.Rd'
grab_cache = TRUE

JULES_control     =  "data/JULES-mort/mort0/"
JULES_experiments =  paste0("data/JULES-mort/", c("mort1", "mortr"))
Experiment_names  = c("frac_min = 1e-6", "frac_min = 0.1")

######################
## open				##
######################
Jules_TC_fire_off = openJulesTree(JULES_control, annual_average = FALSE)

Jules_fire = lapply(JULES_experiments, openJulesTree,  1, "burnt_area_gb", annual_average = FALSE)# Jules_TC_fire_off = openJulesTree(Jules_fire_off_LU_off_fname)
Jules_fire = lapply(Jules_fire,  function(i) i * 60 * 60 * 24 * 365 )

Jules_TC_fire_on = lapply(JULES_experiments, openJulesTree, annual_average = FALSE)


lambda_from_run <- function(TC_fire_on, TC_fire_off, fire) {
    runLambda <- function(i) {
        rec = lambda_dgvm(TC_fire_off[[i+1]], TC_fire_off[[i]], 
                          TC_fire_on[[i+1]], TC_fire_on[[i]], fire[[i+1]], return_r = TRUE)
        #half = log(2)/lambda
        return(rec)
    }
    rec = layer.apply(1:(nlayers(TC_fire_off)-1), runLambda)
    
    rec = mean(rec) / mean(TC_fire_off[[-1]] - TC_fire_on[[-1]])
    return(rec)
}

rec = mapply(lambda_from_run, Jules_TC_fire_on, fire = Jules_fire, MoreArgs = list(TC_fire_off = Jules_TC_fire_off))
half = lapply(rec, function(i) -log(2)/log(1-i))

dTC = mean(Jules_TC_fire_on[[1]] - Jules_TC_fire_off)

sample4weight <- function(hlf, dtc) {
    mask = !is.na(hlf + dtc)
    vhlf = hlf[mask]; vdtc = dtc[mask]
    smp = sample(abs(vhlf), 2*length(vhlf), replace = TRUE, prob = abs(vdtc))
    return(smp)
}

shalf = lapply(half, sample4weight, dTC)
names(shalf) = Experiment_names
options(scipen = 2)
boxplot(shalf, log = 'y')
mtext(side = 2, 'reaction half life', line = 2.5, cex = 1.1)
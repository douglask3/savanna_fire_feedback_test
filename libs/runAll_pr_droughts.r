runAll_fire_pr_droughts <- function(FUN, ...)
    lapply(fire_datasets, function(i) lapply(pr_datasets, function(j)
           lapply(drought_vars, function(k)
                  FUN(fire_dataset = i, pr_dataset = j, drought_var = k, ...))))

#pr_datasets, function(i) lapply(drought_vars, 
#          function(j) FUN(pr_dataset = i, drought_var = j, ...)))

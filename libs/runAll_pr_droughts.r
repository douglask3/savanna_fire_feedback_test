runAll_pr_droughts <- function(FUN, ...)
    lapply(pr_datasets, function(i) lapply(drought_vars, 
          function(j, i) FUN(pr_dataset = i, drought_var = j, ...), i))

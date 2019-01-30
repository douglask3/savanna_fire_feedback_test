selectOutput <- function(ensembles, item = 1)
	lapply(ensembles, function(j) layer.apply(j, function(i) i[[item]]))
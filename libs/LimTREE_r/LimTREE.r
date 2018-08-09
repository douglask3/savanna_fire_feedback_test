runLimTREE <- function(line, dat = NULL, ...) {
	if (is.null(dat)) 
		dat = loadInputData()
		
	params = read.csv(paramFile, stringsAsFactors=FALSE)[line,]
	params = unlist(params)
	
	out = LimTREE(dat[['MAP']], dat[['MAT']], dat[['SW1']], dat[['SW2']], 
			dat[['BurntArea']], dat[['Drought']], dat[['MTWM']],
			dat[['PopDen']], dat[['urban']], dat[['crop']], dat[['pas']],
			params['trans_d'], params['k_popden'], 
			params['v_drought'], params['v_maxTemp'], params['v_popDen'],
			params['v_crop'], params['v_pas'],
			params['MAP_x0'], params['MAP_k'], params['MAT_x0'], params['MAT_k'], 
			params['SW_x0'], params['SW_k'], params['mort_x0'], params['mort_k'],
			params['ex_x0'], params['ex_k'],
			params['max_T'], ...)
	
	return(out)
}       

loadInputData <- function(remove = NULL) {
	files = list.files('data/')
	files = files[grepl('.nc', files)]
	
	dat = lapply(paste('data', files, sep = '/'), raster)
	names(dat) = unlist(strsplit(files, '.nc'))
	
	if (!is.null(remove))
		for (i in remove) dat[[i]][!is.na(dat[[i]])] = 0.0
	
	return(dat)
}


LimTREE <- function(MAP, MAT, SW1, SW2, fire, drought, maxTemp, popDen, urban, crop, pas,
			        d, k_popDen, v_drought, v_maxTemp, v_popDen, v_crop, v_pas,
					MAP0, MAPk, MAT0, MATk, SW0, SWk, Mort0, Mortk, Exc0, Exck, maxT) {
				
	popDen = 1 - exp(popDen * (-1/k_popDen))
	f_MAP  = LimMAP  (MAP, MAP0 , MAPk)
	f_MAT  = LimMAT  (MAT, MAT0 , MATk)
	f_SW   = LimSW   (SW1, SW2  , d, 
						   SW0  , SWk )
	f_Mort = LimMort (fire, drought, maxTemp, popDen, v_drought, v_maxTemp, v_popDen,
						   Mort0, -Mortk)
	f_Exc  = LimExc  (urban, crop, pas, v_crop, v_pas, Exc0, -Exck)
	
	Tree = f_MAP * f_MAT * f_SW * f_Mort * f_Exc * maxT

	
	return(addLayer(Tree, f_MAP, f_MAT, f_SW, f_Mort, f_Exc))
}

logistic <- function(x, x0, k) 
	1 / (1 + exp(-k * (x - x0)))

LimMAP <- LimMAT <- function(...)  logistic(...)

LimList <- function(x, v, ...) {
	v = c(1, v)
	x = mapply('*', x, v)
	xi = x[[1]]
	for (i in x[-1]) xi = xi + i
	
	xi = xi / sum(v)
	out = logistic(xi, ...)
	return(out)
}

LimSW   <- function(SW1, SW2, d, ...)
	LimList(c(SW1, SW2), d, ...)

LimMort <- function(fire, drought, maxTemp, popDen, v_drought, v_maxTemp, v_popDen, ...)
	LimList(c(fire, drought, maxTemp, popDen), c(v_drought, v_maxTemp, v_popDen), ...)

LimExc  <- function(urban, crop, pas, v_crop, v_pas, ...)
	LimList(c(urban, crop, pas), c(v_crop, v_pas), ...)




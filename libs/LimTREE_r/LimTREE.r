runLimTREE <- function(line, paramFile, dat = NULL, ...) {
	if (is.null(dat)) 
		dat = loadInputData()
        
	params = read.csv(paramFile, stringsAsFactors=FALSE)[line,]
	params = unlist(params)
	
	out = LimTREE(dat[['MAP']], dat[['MAT']], dat[['SW1']], dat[['SW2']], 
			dat[['BurntArea']], dat[['Drought']], dat[['MTWM']],
			dat[['PopDen']], dat[['urban']], dat[['crop']], dat[['pas']],
			params['min_mat'], params['max_mat'], params['trans_d'], params['p_fire'], params['k_popden'], params['p_drought'],
			params['min_maxTemp'], params['max_maxTemp'], params['p_maxTemp'],
			params['v_drought'], params['v_maxTemp'], params['v_popDen'],
			params['v_crop'], params['v_pas'],
			params['MAP_x0'], params['MAP_k'], params['MAT_x0'], params['MAT_k'], 
			params['SW_x0'], params['SW_k'], params['mort_x0'], params['mort_k'],
			params['ex_x0'], params['ex_k'],
			params['max_T'], ...)
	
	return(out)
}

LimTREE <- function(MAP, MAT, SW1, SW2, fire, drought, maxTemp, popDen, urban, crop, pas,
			        min_mat, max_mat, d, p_fire, k_popDen, p_drought, min_maxTemp, max_maxTemp, p_maxTemp,
					v_drought, v_maxTemp, v_popDen, v_crop, v_pas,
					MAP0, MAPk, MAT0, MATk, SW0, SWk, Mort0, Mortk, Exc0, Exck, maxT,
					includeSW = FALSE, ...) {
	
	f_MAP  = LimMAP  (MAP, MAP0 , MAPk, ...)
	f_MAT  = LimMAT  (MAT, min_mat, max_mat, MAT0 , MATk, ...)
	
	if (includeSW) f_SW   = LimSW(SW1, SW2  , d,  SW0  , SWk, ...)
	else {
		f_SW = f_MAT
		f_SW[!is.na(f_SW)] = 1.0
	}
	
	f_Mort = LimMort (fire, drought, maxTemp, popDen, v_drought, v_maxTemp, v_popDen,
					  p_fire, k_popDen, p_drought, min_maxTemp, max_maxTemp, p_maxTemp,
					  Mort0, -Mortk, ...)
						   
	f_Exc  = LimExc  (urban, crop, pas, v_crop, v_pas, Exc0, -Exck, ...)
	
	Tree = f_MAP * f_MAT * f_SW * f_Mort * f_Exc * maxT
	
	return(addLayer(Tree, f_MAP, f_MAT, f_SW, f_Mort, f_Exc))
}

logistic <- function(x, x0, k, sensitivity = FALSE) {
	FUN <- function(xi = x, ki = k)  1 / (1 + exp(-ki * (xi - x0)))
	dFUN <- function(xi) FUN(xi, k) * FUN(xi, -k)
	if (sensitivity)
		out = dFUN(x)/ dFUN(x0)
	else
		out = FUN()
	return(out)
}

LimMAP <- function(...)  logistic(...)

LimMAT <- function(MAT, min_mat, max_mat, ..., retutnVar = FALSE) {
	MAT = MAT - min_mat
	max_mat = max_mat - min_mat
	#MAT = MAT / max_mat
	
	MAT[MAT <0] = 0
	#MAT[MAT >1] = 1
	
	MAT = logmin(MAT)
	if (retutnVar) return(MAT)
	logistic(MAT, ...)
}

LimList <- function(x, v, ..., retutnVar = FALSE) {
	v = c(1, v)
	x = mapply('*', x, v)
	xi = x[[1]]
	for (i in x[-1]) xi = xi + i
	
	xi = xi / sum(v)
	if (retutnVar) out = xi else out = try(logistic(xi, ...), silent = TRUE)
	if (class(out) =="try-error") out = xi
	return(out)
}

LimSW   <- function(SW1, SW2, d, ...)
	LimList(c(SW1, SW2), d, ...)
	
LimTREE.popDen <- function(popDen, k) 
	1 - exp(popDen * (-1/k))
	
LimTREE.maxTemp <- function(maxTemp, mn, mx, p) {
	maxTemp = (maxTemp - mn)/(mx - mn)
	maxTemp[maxTemp < 0] = 0
	maxTemp[maxTemp > 1] = 1
	
	maxTemp = maxTemp ^ p
	
	return(maxTemp)
}


LimMort <- function(fire, drought, maxTemp, popDen, v_drought, v_maxTemp, v_popDen,
				    p_fire, k_popDen, p_drought, min_maxTemp, max_maxTemp, p_maxTemp,...) {
		
	fire = fire^p_fire
	
	popDen  = LimTREE.popDen(popDen, k_popDen)
	
	maxTemp = LimTREE.maxTemp(maxTemp, min_maxTemp, max_maxTemp, p_maxTemp)
	drought = drought^p_drought
	LimList(c(fire, drought, maxTemp, popDen), c(v_drought, v_maxTemp, v_popDen), ...)
}

LimExc  <- function(urban, crop, pas, v_crop, v_pas, ...)
	LimList(c(urban, crop, pas), c(v_crop, v_pas), ...)




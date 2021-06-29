runLimTREE <- function(line, paramFile, dat = NULL, ...) {
    if (is.null(dat)) 
	dat = loadInputData()
    
    params = read.csv(paramFile, stringsAsFactors=FALSE)[line,]
    params = unlist(params)
    
    out = LimTREE(dat[['MAP']], dat[['RainTerm_Drought']],
                  dat[['SW1']], dat[['SW2']], 
                  dat[['BurntArea']], dat[['StressTerm_Drought']], dat[['MTWM']],
                  dat[['MTCM']],
                  dat[['PopDen']],
                  dat[['buffalo']], dat[['cattle']], dat[['goat']], dat[['sheep']],
                  dat[['urban']], dat[['crop']], dat[['pas']],
                  params['m_drought'],
                  params['min_mat'], params['max_mat'],
                  params['trans_d'], params['p_fire'],
                  params['k_pop'],
                  params['k_buffalo'], params['k_cattle'], 
                  params['k_goat'], params['k_sheep'],
                  params['p_drought'],
                  params['min_maxTemp'], params['max_minTemp'],
                  params['q10_maxTemp'], params['q10_minTemp'],
                  params['v_drought'], params['v_maxTemp'], params['v_minTemp'],
                  params['v_pop'],
                  params['v_buffalo'], params['v_cattle'],
                  params['v_goat'], params['v_sheep'],
                  params['v_crop'] ,params['v_pas'],
                  params['MAP_x0'], params['MAP_k'], params['MAT_x0'], params['MAT_k'], 
                  params['SW_x0'], params['SW_k'], params['mort_x0'], params['mort_k'],
                  params['ex_x0'], params['ex_k'],
                  params['max_T'], ...)
	
    return(out)
}

LimTREE <- function(MAP, rain_drought, SW1, SW2, fire, stress_drought,
                    maxTemp, minTemp,
                    popDen, 
                    buffalo, cattle, goat, sheep,
                    urban, crop, pas,
		    m_drought, min_mat, max_mat,
                    d, p_fire,
                    k_popDen, k_buffalo, k_cattle, k_goat, k_sheep,
                    p_drought, min_maxTemp, max_minTemp,
                    q10_maxTemp, q10_minTemp,
		    v_drought, v_maxTemp, v_minTemp,
                    v_popDen, v_buffalo, v_cattle, v_goat, v_sheep,
                    v_crop, v_pas, 
		    MAP0, MAPk, MAT0, MATk, SW0, SWk, Mort0, Mortk, Exc0, Exck, maxT,
		    includeSW = TRUE, ...) {
    
    f_MAP  = LimMAP  (MAP, rain_drought, m_drought, MAP0 , MAPk, ...)
    #f_MAT  = LimMAT  (MAT, min_mat, max_mat, MAT0 , MATk, ...)
	
    if (includeSW) f_SW   = LimSW(SW1, SW2  , d, SW0  , SWk, ...)
    else {
	f_SW = f_MAP
	f_SW[!is.na(f_SW)] = 1.0
    }

    f_MAT = f_MAP
    f_MAT[!is.na(f_MAT)] = 1.0
	
    f_Mort = LimMort (fire, stress_drought, maxTemp, minTemp, popDen,
                      buffalo, cattle, goat, sheep,
                      v_drought, v_maxTemp, v_minTemp,
                      v_popDen, v_buffalo, v_cattle, v_goat, v_sheep,
                      p_fire, 
                      k_popDen, k_buffalo, k_cattle, k_goat, k_sheep,
                      p_drought,
                      min_maxTemp, max_minTemp, q10_maxTemp, q10_minTemp,
                      Mort0, -Mortk, ...)

    			   
    f_Exc  = LimExc  (urban, crop, pas, v_crop, v_pas, Exc0, -Exck, ...)
    
    f_SW  = raster::resample(f_SW , f_MAP)
    f_Exc = raster::resample(f_Exc, f_MAP)
    Tree = f_MAP * f_SW * f_Mort * f_Exc * maxT #  * f_MAT
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

LimMAP <- function(MAP, drought, m_drought,...){
    #MAP = MAP + (1-drought) * (1/m_drought) * (exp(-m_drought * MAP)-1)
    MAP = log(MAP)
    logistic(MAP, ...)
}


LimMAT <- function(MAT, min_mat, max_mat, ..., retutnVar = FALSE) {
    MAT = MAT - min_mat
    max_mat = max_mat - min_mat
	
    MAT[MAT <0] = 0
	
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

LimSW   <- function(SW1, SW2, d,...) {
    SW = (SW1 + d * SW2)/(1+d)
    #SW = logmin(SW)    
    
    logistic(SW, ...)
}

LimTREE.convertUnity <- function(x, k) 
    1 - exp(x * (-1/k))
	
LimTREE.Temp <- function(Temp, mn, q10) {
    Temp = (q10 + 1.0)^(Temp - mn)	
    return(Temp)
}


LimMort <- function(fire, drought, maxTemp, minTemp, popDen,
                    buffalo, cattle, goat, sheep,
                    v_drought, v_maxTemp, v_minTemp,
                    v_popDen, v_buffalo, v_cattle, v_goat, v_sheep,
		    p_fire,
                    k_popDen, k_buffalo, k_cattle, k_goat, k_sheep,
                    p_drought, min_maxTemp, max_minTemp,
                    q10_maxTemp, q10_minTemp, ...) {
		
    fire = fire^p_fire
    popDen0 = popDen
    popDen   = LimTREE.convertUnity( popDen, k_popDen )
    buffalo  = LimTREE.convertUnity(buffalo, k_buffalo)
    cattle   = LimTREE.convertUnity( cattle, k_cattle )
    goat     = LimTREE.convertUnity(   goat, k_goat   )
    sheep    = LimTREE.convertUnity(  sheep, k_sheep  )
    
    maxTemp = LimTREE.Temp(maxTemp     ,  min_maxTemp, q10_maxTemp)
    minTemp = LimTREE.Temp(minTemp*(-1), -max_minTemp, q10_minTemp)
    drought = drought^p_drought
    
    LimList(c(fire, drought, maxTemp, minTemp,
              popDen, buffalo, cattle, goat, sheep),
            c(v_drought, v_maxTemp, v_minTemp,
              v_popDen, v_buffalo, v_cattle, v_goat, v_sheep), ...)
}

LimExc  <- function(urban, crop, pas, v_crop, v_pas, ...)
    LimList(c(urban, crop, pas), c(v_crop, v_pas), ...)




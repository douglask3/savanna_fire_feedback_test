plot_4way <- function(x, y, A, B, C, D, x_range = c(-180, 180), y_range = c(-90, 
    90), limits = c(0.1, 0.5, 0.9), cols = c("FF", "CC", "99", 
    "55", "11"), add_legend = TRUE, smooth_image = FALSE, smooth_factor = 5, 
    add = FALSE, normalise = TRUE, ePatternRes = 30, ePatternThick = 1/3, ...) 
{
	
    remove_nans <- function (x, y, A, B, C, D) {
        test = is.na(A + B + C + D) == FALSE
        A = A[test]
        B = B[test]
        C = C[test]
        D = D[test]
        x = x[test]
        y = y[test]
        return(list(x, y, A, B, C, D))
    }
    
    disagg_xyabcd <- function (x, y, A, B, C, D, smooth_factor) {
        `:=`(c(nn, nn, A), disagg_xyz(x, y, A, smooth_factor = smooth_factor))
        `:=`(c(nn, nn, B), disagg_xyz(x, y, B, smooth_factor = smooth_factor))
        `:=`(c(x, y, C), disagg_xyz(x, y, C, smooth_factor = smooth_factor))
        `:=`(c(x, y, D), disagg_xyz(x, y, D, smooth_factor = smooth_factor))
        return(list(x, y, A, B, C, D))
    }

    ncols = length(cols)

    #mag = A^2 + B^2 + C^2
    
    #A = sqrt(A^2/mag)
    #B = sqrt(B^2/mag)
    #C = sqrt(C^2/mag)
    #D = sqrt(D^2/mag)
    
	if (normalise) {
		mag = A + B + C
	
		A = A/mag
		B = B/mag
		C = C/mag
		D = D/mag
	
		A[mag == 0] = 0.33
		B[mag == 0] = 0.33
		C[mag == 0] = 0.33
		D[mag == 0] = 0.33
    } else {
		Ai = (B + C)/2
		Bi = (A + C)/2
		Ci = (A + B)/2
		A = Ai
		B = Bi
		C = Ci
	}
	
    out = rasterFromXYZ(cbind(x, y, D))
	
    out = addLayer(out, rasterFromXYZ(cbind(x, y, B)),
                        rasterFromXYZ(cbind(x, y, C)),
                        rasterFromXYZ(cbind(x, y, D)))
                        
    if (smooth_image) {
        `:=`(c(x, y, A, B, C, D), disagg_xyabc(x, y, A, B, C, D, smooth_factor))
    }
    `:=`(c(x, y, A, B, C, D), remove_nans(x, y, A, B, C, D))
    
    Az = cut_results(A, limits)
    Bz = cut_results(B, limits)
    Cz = cut_results(C, limits)
    Dz = cut_results(D, limits)
	
    z = 1:length(Az)
    zcols = paste("#", cols[Az], cols[Bz], cols[Cz], sep = "")
    #zcols = darken(zcols)
	#zcols = saturate(zcols, 0.1)
    #zcols = mapply(lighten, zcols    )
    #zcols = mapply( darken, zcols, Dz)
    
	
    
    z = rasterFromXYZ(cbind(x, y,  z))
    e = rasterFromXYZ(cbind(x, y, length(limits) + 2 - Dz))
    
    lims = (min.raster(z, na.rm = TRUE):max.raster(z, na.rm = TRUE) -  0.5)[-1]
    
    plotFun <- function(add) plot_raster_from_raster(z, cols = zcols[sort(unique(z))], 
        limits = lims, x_range = x_range, y_range = y_range, 
        quick = TRUE, readyCut = TRUE, 
        add_legend = FALSE, add = add, 
        e = e, limits_error = 0.5 + 1:length(limits),  
        ePatternRes = ePatternRes,  ePatternThick = ePatternThick, e_polygon = FALSE,
        ...)
    #plot_raster_from_raster(x, y_range = y_range,
	#						cols = cols, limits = limits,
	#						e = e, limits_error = limits_error, 
	#						ePatternRes = 30, ePatternThick = 0.2,
	#						quick = TRUE, add_legend = FALSE)
	
    plotFun(add)
    
    if (add_legend) {
        add_raster_4way_legend(cols, limits, ...)            
        plotFun(TRUE)
    }
	add_icemask()
    return(out)
}
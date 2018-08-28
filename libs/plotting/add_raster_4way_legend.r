add_raster_4way_legend <- function(labs = letters[1:4], limits = c(0.25, 0.5, 0.75),
                                   nx = ny * 12.5, ny = 200, ...) {
    
    limits5 = c(0, limits, 1)
    add_3way <- function(i, xpos) {
        out = add_raster_3way_legend_for_4way(legend, xpos = xpos, limits = limits, hash = i, ...)      
        
        #text(x = mean(xpos) * nx, y = -0.45 * ny, paste(limits5[i], '<', labs[4], '<', limits5[i+1]), xpd = TRUE)
        return(rbind(legend.xyz, out))  
    }

    plot(c(0, nx), c(0, ny), type = 'n', axes = FALSE, xlab = '', ylab = '')
    legend = raster(nrows=ny, ncols=nx, xmn = 0, xmx = 1, ymn = 0, ymx =1)
    legend.xyz = c()
    
    legend.xyz = add_3way(1, xpos = c(0.00, 0.20))    
    legend.xyz = add_3way(2, xpos = c(0.25, 0.45)) 
    legend.xyz = add_3way(3, xpos = c(0.50, 0.70))    
    legend.xyz = add_3way(4, xpos = c(0.75, 0.95))     
    
    legend.z = rasterFromXYZ(apply(legend.xyz[,  1:3   ],2,as.numeric))
    legend.e = rasterFromXYZ(apply(legend.xyz[,c(1:2,4)],2,as.numeric))
    cols = unique(legend.xyz[,5])
    limits = as.numeric(unique(legend.xyz[,3])[-1]) - 0.5
    
    plot_raster_from_raster(legend.z, cols = cols, limits = limits, smooth_image = FALSE, coast.lwd = NULL, 
                             add_legend = FALSE, e = legend.e, invert_e = FALSE,  limits_error = seq(1.5, 3.5),
                             add = TRUE,  ePatternRes = 100,  ePatternThick = 0.4, e_polygon = FALSE)
    

    #text(x = 0.10 * nx, y = -0.25 * ny, labs[1], xpd = TRUE           )
    #text(x = 0.00 * nx, y =  0.50 * ny, labs[2], xpd = TRUE, srt =  45)
    #text(x = 0.20 * nx, y =  0.50 * ny, labs[3], xpd = TRUE, srt = -45)
    
    #text(x = seq(0.0, 0.2*nx, length.out = 5), y = -0.1 *ny, rev(limits5), xpd = TRUE)    
}

add_raster_3way_legend_for_4way <- function(legend.z, cols, limits = c(0.25, 0.5, 0.75), xpos = c(0,1), 
                                    lighten_factor = 1.4, hash = 1,...) {
    
    nsq = nrow(legend.z)
    limits = limits * nsq
    xpos = xpos * ncol(legend.z)
    
    l = 0; u = nsq; xyz = c()
    nz = 0
    for (i in 1:nsq) {
        if (i %% 2 == 1) l = l + 1
            else         u = u - 1
        
        z = (l:u)
        
        x = (z / nsq * diff(xpos)) + xpos[1]
        
        y = rep(i, length(x))
        
        bl = nsq - z - floor(i/2) + 1 
        gr = rep(i, length(z))
        rd = z - floor(i/2)
        
        cut_results_col <- function(x) {
            x = cut_results(x, limits)
            x = cols[x]
            return(x)
        }
        
        bl = cut_results_col(bl)
        gr = cut_results_col(gr)
        rd = cut_results_col(rd)
        
        xcols = paste('#', rd, gr, bl, sep = '')        
        xyz = rbind(xyz, cbind(x, y, NaN, hash, xcols))
    }
    
    ucols = unique(xyz[,5])
    xyz[,3] = sapply(xyz[,5], function(i) which(i == ucols))
    
    return(xyz)
}
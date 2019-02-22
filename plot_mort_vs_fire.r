######################
## cfg				##
######################
source("cfg.r")
graphics.off()

dat = loadInputData()
paramFile = paste0(paramFile, '_', pr_datasets[1], '_', drought_vars[1], '.csv')
params = read.csv(paramFile, stringsAsFactors=FALSE)

temp_file = 'temp/plot_mort_dat2.Rd'
grab_cache = TRUE

fireMin = 0.0005
treeMin = 0.0005


JULES_control     =  "data/JULES-mort/mort0/"
JULES_experiments =  paste0("data/JULES-mort/", c("mort1",  "mortc", "mortr", 
                            "respv/experiment", "resp0/experiment"), '/')
Experiment_names  = c("100% mortality", "PFT-specific + crop masking", 
                      "frac_min=0.1", "scaled resp + PFT-specfic mort", "scaled resp 100% mort")

######################
## open				##
######################	
## Inferened
if (file.exists(temp_file) & grab_cache) {
	load(temp_file)
} else {
	out = makeOrLoadEnsembles()	
	out = selectOutput(out)

	## Jules
	Jules_TC_fire_off = openJulesTree(JULES_control)
	Jules_PFTs = openJulesTree(JULES_control, splitPFTs = TRUE)
	Jules_fire = layer.apply(JULES_experiments, openJulesTree,  1, "burnt_area_gb") * 60 * 60 * 24 * 365 # Jules_TC_fire_off = openJulesTree(Jules_fire_off_LU_off_fname)
	Jules_fire = raster::resample(Jules_fire, Jules_TC_fire_off)
	Jules_TC_fire_on = layer.apply(JULES_experiments, openJulesTree)
		
    
	Jules_dout = (Jules_TC_fire_off - Jules_TC_fire_on)/max(addLayer(Jules_TC_fire_off, Jules_TC_fire_on))		
	Jules_dout =  squeeze(Jules_dout, 500)
	#Jules_dout[ Jules_dout < treeMin] = treeMin + treeMin/10
	dout = lapply(out, function(i) 1-out[[1]]/i)
	
	save(out, Jules_TC_fire_off, Jules_fire, Jules_TC_fire_on, Jules_dout, dout, Jules_PFTs, file = temp_file)
}

######################
## plot				##
######################
do_the_plot <- function(jules_fire, jules_dout, yaxis = FALSE, addLegend = FALSE, title = '', normalise = FALSE, log = '')  {
    if (log != 'xy') 
        treeMin = -1
    #else
    #    jules_dout[ jules_dout < treeMin] = treeMin + treeMin/10
    
	mask = !is.na(jules_fire + jules_dout) & jules_fire > fireMin & jules_dout > treeMin
    
    
    addPFT <- function(i) {
        plot(c(fireMin, 1), c(treeMin, 1), axes = FALSE, xlab = '', ylab = '', log = log, type ='n')#, xaxs = 'i', yaxs = 'i'
        if (i == 1) {
            mtext(title, line = 2, side = 2)
            axis(2)
        }
        if (yaxis) axis(1)
        
        pfts_names = c("All Trees", "BDT", "BET-Tr", "BET-Te", "NDT", "NET", "DSh", "ESh")
        if (addLegend) mtext(side = 3, pfts_names[i])
        
        labs = c(0.00001, 0.00003, 0.0001, 0.0003, 0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 1)	#labs = seq(0, 100, 20)/100
        labels = labs * 100
        nolabels = rep('', length(labs))
        
        addPolygon <- function(y, x = dat[["BurntArea"]]) {  # dat[['crop']] + dat[['pas']]
            quantileDesnityPoly(x[], y[], xlim = c(0, 1), quantiles = c(0.0 , 1.0), between = TRUE, xlog = TRUE, col = make.transparent('black', 0.99))
            quantileDesnityPoly(x[], y[], xlim = c(0, 1), quantiles = c(0.1 , 0.9), between = TRUE, xlog = TRUE, col = make.transparent('#330000', 0.99))
            quantileDesnityPoly(x[], y[], xlim = c(0, 1), quantiles = c(0.25, 0.75), between = TRUE, xlog = TRUE, col = make.transparent('red', 0.98))
        }
        layer.apply(dout[[5]] / 0.8, addPolygon) #12
        x = 10^(seq(-5, 1, 0.01))
        lines(x,x, lty = 2,lwd = 2)
	
	
        pft_frac = Jules_PFTs[[i]]   
    
        x = jules_fire[mask]
        y = jules_dout[mask]
        w = pft_frac[mask]
        w[is.na(w)] = 0.0
       
        
        if (log == 'xy') {
            k <- kde2d(log10(x), log10(y), n = c(100, 200))
            k[[1]] = 10^k[[1]]
            k[[2]] = 10^k[[2]]
        } else {
            k <- kde2d(jules_fire[mask], jules_dout[mask], n = c(100, 200))
        }
        if (normalise) {
            ni = 5
            
            #browser()
            #k[[3]] = apply(k[[3]], 2, function(i) i/sum(i))
            k[[3]] = t(apply(k[[3]], 1, function(i) (i/sum(i))))
            cols = rep("#0000FF", 100)
            cols = mapply(make.transparent, cols, seq(1.0, 0.5, length.out = length(cols)))
            for (i in 1:ni) image(k, col=cols, add = TRUE)
        } else {
            x0 = x
            y0 = y
            index = sample(1:length(x), size = length(x)*3, prob = w, replace = TRUE)
            x = x[index]
            y = y[index]

            cols = unique(unlist(lapply(1:6,function(i) make_col_vector(rev(blues9)[i:(i+1)], ncols = 8-i))))
            cols = unlist(lapply(1:8,function(i) rep(rev(blues9)[i], 9-i)))
            cols = densCols(log10(x),log10(y), colramp = colorRampPalette(rev(cols)), bandwidth = 0.02)
            points(y~x, pch = 20, cex = 2, col = make.transparent(cols, 0.67))
        }	

    }
    Jules_PFTs = addLayer(sum(Jules_PFTs), Jules_PFTs) 
    Jules_PFTs = Jules_PFTs/Jules_PFTs[[1]]
     
    layer.apply(1:nlayers(Jules_PFTs), addPFT)
    
	if (addLegend) {
		legendFun <- function(col1, col2 = '#00000000', lwd, bty = 'n')
				   legend('bottomright', legend = c('JULES', 'Bayes', '1:1'),
							  col = c(col2, col1, 'black'),
							  lty = c(1, 1, 2), lwd = c(lwd, lwd, 2), bty =bty)

		legendFun(make.transparent('black', 0.99), lwd = 21, bty = '0')
		for (i in 1:(floor(length(ensemble_no))/3)) {
			   legendFun(make.transparent('black', 0.99), make.transparent('blue', 0.99), lwd = 19.5)
			   legendFun(make.transparent('black', 0.99), make.transparent('blue', 0.99), lwd = 15)
			   legendFun(make.transparent('#330000', 0.99), make.transparent('blue', 0.99), lwd = 12)
			   legendFun(make.transparent('#330000', 0.99), make.transparent('blue', 0.99), lwd = 9)
			   legendFun(make.transparent('red', 0.98), make.transparent('blue', 0.98), lwd = 6)
			   legendFun(make.transparent('red', 0.98), make.transparent('blue', 0.98), lwd = 3)
		 }
	}   
}

plot_the_plot <- function(normalise, log = '') {
	fname = paste('figs/fire_impact-normalise', normalise, log,'.png', sep = '')
    nrows =  nlayers(Jules_PFTs) + 1; ncols = length(Experiment_names)
	png(fname, width = 3.25 * nrows, height = 3.25 * ncols, res = 300, unit = 'in')
		par(mar = c(1, 1, 1.5, 0), oma = c(2.5, 5, 0, 1), mfrow = c(ncols, nrows))
        
        y_axis = rep(c(T,F), length.out = nlayers(Jules_dout))
        y_axis = c(rep(F, length.out = nlayers(Jules_dout)-1), T)
        leg_ts = c(T, rep(F, length.out = nlayers(Jules_dout) - 1))
        
		do_the_plots <- function(i, ...) do_the_plot(Jules_fire[[i]], Jules_dout[[i]], ..., normalise = normalise, log = log)
		mapply(do_the_plots, 1:nlayers(Jules_dout), y_axis, leg_ts,
			  Experiment_names)
		
		mtext(side = 1, 'Burnt Area (%)'          , line = 1.5, outer = TRUE) #Land Use (%)
		mtext(side = 2, 'Impact on Tree Cover (%)', line = 4, outer = TRUE)
	dev.off()#.gitWatermark(x = 1.01)
}

#plot_the_plot(TRUE)
plot_the_plot(FALSE)
plot_the_plot(FALSE, log = 'xy')

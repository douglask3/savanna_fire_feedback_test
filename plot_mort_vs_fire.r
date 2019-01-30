######################
## cfg				##
######################
source("cfg.r")
graphics.off()

dat = loadInputData()
params = read.csv(paramFile, stringsAsFactors=FALSE)

temp_file = 'temp/plot_mort_dat.Rd'
grab_cache = TRUE

fireMin = 0.0005
treeMin = 0.0005


JULES_control     =  "data/JULES-mort/mort0/"
JULES_experiments =  paste0("data/JULES-mort/", c("mort1", "mortv", "mortc"))
Experiment_names  = c("100% mortality", "PFT-specific mortaility", "PFT-specific + crop masking")

linear.bounded <- function(x, a, b, minY = 0, maxY = 1) {
	y = a * x + b 
	y[y>maxY] = maxY
	y[y<minY] = minY
	return(y)
}

squeeze <- function(x, ns = 100) x = ((ns - 1) * x + 0.5)/ns
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
	#dout = lapply(out, function(i) 1-out[[1]]/i)
	
	save(out, Jules_TC_fire_off, Jules_fire, Jules_TC_fire_on, Jules_dout, dout, Jules_PFTs, file = temp_file)
}

######################
## plot				##
######################
do_the_plot <- function(jules_fire, jules_dout, yaxis = FALSE, addLegend = FALSE, title = '', normalise = FALSE, log = '')  {
    if (log != 'xy') 
        treeMin = -1
    else
        jules_dout[ jules_dout < treeMin] = treeMin + treeMin/10
    
	mask = !is.na(jules_fire + jules_dout) & jules_fire > fireMin & jules_dout > treeMin
    
    
    addPFT <- function(i) {
        plot(c(fireMin, 1), c(treeMin, 1), axes = FALSE, xlab = '', ylab = '', xaxs = 'i', yaxs = 'i', log = log, type ='n')
        if (i == 1) {
            mtext(title, line = 2.5, side = 2)
            axis(2)
        }
        if (yaxis) axis(1)
        
        pfts_names = c("All Trees", "BDT", "BET-Tr", "BET-Te", "NDT", "NET", "DSh", "ESh")
        if (addLegend) mtext(side = 3, pfts_names[i])
        
        labs = c(0.00001, 0.00003, 0.0001, 0.0003, 0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 1)	#labs = seq(0, 100, 20)/100
        labels = labs * 100
        nolabels = rep('', length(labs))
        
       # add_axis <- function(test, side, lab) {
       # 
       #     if (test) labels = labels
       #         else labels = nolabels
       # 
       #     axis(side, at = labs, labels = labels) 
       # }
        
       # add_axis(xaxis, 1, 'Burnt Area (%)')
       # add_axis(yaxis, 2, 'Impact on Tree Cover (%)')
        
        addPolygon <- function(y, x = dat[["BurntArea"]]) {  # dat[['crop']] + dat[['pas']]
            quantileDesnityPoly(x[], y[], xlim = c(0, 1), quantiles = c(0.0 , 1.0), between = TRUE, xlog = TRUE, col = make.transparent('black', 0.99))
            quantileDesnityPoly(x[], y[], xlim = c(0, 1), quantiles = c(0.1 , 0.9), between = TRUE, xlog = TRUE, col = make.transparent('#330000', 0.99))
            quantileDesnityPoly(x[], y[], xlim = c(0, 1), quantiles = c(0.25, 0.75), between = TRUE, xlog = TRUE, col = make.transparent('red', 0.98))
        }
        layer.apply(dout[[5]] / 0.8, addPolygon) #12
        x = 10^(seq(-5, 1, 0.01))
        #lines(x, 1-logistic(x, params[1, 'mort_x0'], -params[1, 'mort_k'])/logistic(0, params[1, 'mort_x0'], -params[1, 'mort_k']), col = 'red')
        lines(x,x, lty = 2,lwd = 2)
	
	
        jules_dout = jules_dout[[i]]
        jules_fire = raster::crop(jules_fire, extent(-180, 180, -30, 30))
        jules_dout = raster::crop(jules_dout, extent(-180, 180, -30, 30))
        mask = raster::crop(mask, extent(-180, 180, -30, 30))
        
        if (log == 'xy') {
            k <- kde2d(log10(jules_fire[mask]), log10(jules_dout[mask]), n = c(100, 200))
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
        } else ni = 4
        
        #points(jules_fire[], jules_dout[], pch = 19 , col = make.transparent(col, 0.9), cex = 1)	
        cols = rep("#0000FF", 100)
        cols = mapply(make.transparent, cols, seq(1.0, 0.5, length.out = length(cols)))
        
       
        for (i in 1:ni) image(k, col=cols, add = TRUE)
	}
    jules_dout = addLayer(jules_dout, jules_dout * Jules_PFTs)
     
    layer.apply(1:nlayers(jules_dout), addPFT)
    
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
		#for (i in 1:10) legendFun(col1 = '#00000000', make.transparent('blue', 0.8), i)
	}   
	#x = log10(jules_fire[mask])#jules_fire[mask]#
	#y = jules_dout[mask]
	
	#fit = nls(y ~ a * logistic(x, x0, k), start = c(x0 = -1, k = 6, a = 1), lower = c(-3, 0, a = 0.5), algorithm = "port")
	#fit = lm(y~x)
		
	#xp = seq(-5, 1, length.out = 1001)#seq(0, 100, 1)
	#yp = predict(fit, data.frame(x = xp))
		
	#xp = 10^xp
	#lines(xp, yp, col = col, lwd = 2)
}

plot_the_plot <- function(normalise, log = '') {
	fname = paste('figs/fire_impact-normalise', normalise, log,'.png', sep = '')
    nrows =  nlayers(Jules_PFTs) + 1; ncols = length(Experiment_names)
	png(fname, width = 3.25 * nrows, height = 3.25 * ncols, res = 300, unit = 'in')
		par(mar = c(1, 1, 1.5, 0), oma = c(2.5, 3, 0, 1), mfrow = c(ncols, nrows))
        
        y_axis = rep(c(T,F), length.out = nlayers(Jules_dout))
        y_axis = c(rep(F, length.out = nlayers(Jules_dout)-1), T)
        leg_ts = c(T, rep(F, length.out = nlayers(Jules_dout) - 1))
        
		do_the_plots <- function(i, ...) do_the_plot(Jules_fire[[i]], Jules_dout[[i]], ..., normalise = normalise, log = log)
		mapply(do_the_plots, 1:nlayers(Jules_dout), y_axis, leg_ts,
			  Experiment_names)
		
		mtext(side = 1, 'Burnt Area (%)'          , line = 1.5, outer = TRUE) #Land Use (%)
		mtext(side = 2, 'Impact on Tree Cover (%)', line = 1.5, outer = TRUE)
	dev.off.gitWatermark(x = 1.01)
}

plot_the_plot(TRUE)
#plot_the_plot(FALSE, log = 'xy')
plot_the_plot(FALSE)
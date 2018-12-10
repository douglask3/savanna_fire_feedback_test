########################################
## cfg							      ##
########################################
source("cfg.r")
graphics.off()

precip_dir = '~/../Google Drive/disturbance/processed rainfall data/overlap period 2000-2013/'

products = list.files(precip_dir)
products = products[!grepl('.ini', products)]

processProduct <- function(product) {
	files = list.files(paste(precip_dir, product, sep = '/'), full.names = TRUE)
	files = files[!grepl('.ini',files)]

	process_file <- function(file) {
		dat = read.csv(file, stringsAsFactors = FALSE)
		
		z = dat[,-(1:2)]
		if (class(z) == "data.frame") z = apply(dat[,-(1:2)], 1, mean)
		z[is.na(z)] = 1
		r = rasterFromXYZ(cbind(dat[2:1], z))
		
		if (!all(res(r) == c(0.5, 0.5))) r = raster::aggregate(r, 2)
		
		
		vname = filename.noPath(file, noExtension=TRUE)
		
		if          (vname == "01_annual_sum_final") vname = "MAP"
			else if (vname == "02_annual_dryf_final")  vname = "MADD"
			else if (vname == "03_driestmonth_dryf_final") vname = "MDDM"
			else if (vname == "04_pdm_final") {
				r = 1 - r
				vname = "MADM"
		  } else if (vname == "06_seasoanl_concentration_final") vname = "MConc"
			else browser()
		filename = paste0(data_dir, '/', vname, '_',product,'.nc') 		
		names(r) = NULL
		r = writeRaster.gitInfo(r, filename,
								comment = list(source='Based on data the amazing Li G processed for me. Regridded for 0.25 to 0.5', 
										       src_file = 'make_precip.inputs.r',
											   input_file = file), 
								overwrite = TRUE)
										  
		plot(r)
		mtext(paste(product, vname, sep = '-'), side = 3, line = -2)
	}
	dat = lapply(files, process_file)
	return(dat)	
}

lapply(products[2], processProduct)
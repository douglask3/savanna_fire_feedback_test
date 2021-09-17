########################################
## cfg          		      ##
########################################
source("cfg.r")
graphics.off()

biomes = raster("data/biomAssigned.nc")

summaryFileC = "model_summary-nEns-111.nc"
summaryFileE = "model_summary-nEns-diff111.nc"
PostDir = "data/sampled_posterior/attempt15/"

conID  = "control"
expIDs = c("Burnt\narea" = "noFire", "Heat\nStress" = "noTasMort","Wind" = "noWind",
            "Rainfall\ndistribution" = "noDrought", "Population\ndensity" = "noPop",
            "Urban\narea" = "noUrban", "Cropland\narea" = "noCrop", "Pasture\narea" = "noPas",
            "No Humans" = "noHumans")
obsFile = 'data/driving_Data/TreeCover.nc'   

cntr_varnames = c("tree_cover_mean",
                  paste0("potential_", c("map", "energy", "mortality", "exclude")))
names(cntr_varnames) = c("Sim.", "MAP", "Energy", "Mortality", "Exclusion")
#model_summary-nEns-diff101.nc
openDat <- function(id, summaryFile, vanme = "tree_cover_mean", dpr = "") {
    print(id)
    print(vanme)
    print(dpr)
    #if (dpr !="") browser()
    if (vanme == "potential_energy") {
        out1 = openDat(id, summaryFile, "potential_sw", dpr)
        out2 = openDat(id, summaryFile, "potential_mat", dpr)
        return(1-(1-out1)*(1-out2))
    }
    brick.NaN(paste0(PostDir, '/', id, '/', dpr, '/', summaryFile),
              varname = vanme, layers = c(3, 7))
}
run4ConID <- function(dpr) {
    outFile = paste0("outputs/biomeShifts-", dpr, ".csv")
    
    if (file.exists(outFile) & F){
        browser()
    }
    control = lapply(cntr_varnames, function(i) openDat(conID, summaryFileC, i, dpr = dpr))
    obs = raster::resample(raster(obsFile), control[[1]])/0.8
    biomes = raster::resample(biomes, control[[1]])
    experiments = lapply(expIDs, openDat, summaryFileE, dpr = dpr)

    dats = c("Obs." = obs, control, experiments)


    no4Biome <- function(biome = 1, nme) {
        if (biome>1) {
            mask = biomes != biome       
            dats = lapply(dats, function(i) {i[mask] = NaN; i})
        }
        rarea = raster::area(dats[[1]], na.rm = TRUE)
        srarea = sum.raster(rarea, na.rm = TRUE)
        treeArea <- function(r) sum.raster(r * rarea, na.rm = TRUE)/srarea
        sumDat <- function(dat, norm = NULL) {        
            out = 100* unlist(layer.apply(dat, treeArea))
            
            if (length(out) > 1 && !is.null(norm) && any(norm != out))
                out = rbind(out, 100*out/(out+norm))
            if (length(out)==1) out = c(out, NaN)
            if (is.null(dim(out))) names(out) = c("10%", "90%")
            else rownames(out) = c("land", "forest")
            #    names(out) = c("10% - land", "90% - land", "10% - forest", "90% - forest area")
            return(out)
        }
        norm = sumDat(dats[[2]])
        
        out = lapply(dats, sumDat, norm)
    
        out = do.call(rbind, out)
        colnames(out) = paste0(nme, '-', colnames(out))
        rownames(out) = c(rownames(out)[1:2],
                          paste0(rep(c(names(cntr_varnames)[-1], names(experiments)), each = 2), 
                                 '-', rownames(out)[3:nrow(out)]))
        #browser()
        out
    }

    out = mapply(no4Biome, c(1:6, 8),c("Global", "Wet Forests", " Dry Forest", "Savanna/grass", "Mediterranean", "Summergreen Forests/woodland", "Desert/shrub") , SIMPLIFY = FALSE)
    out = do.call(cbind, out)
    out = round(out, 2)

    write.csv(out, outFile)
    return(out)
}

dprs = list.dirs(paste0(PostDir, conID), full.names=FALSE)
run4ConID('')
browser()
outs = lapply(dprs[-1], run4ConID)


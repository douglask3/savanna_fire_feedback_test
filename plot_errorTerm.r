source("cfg.r")

params_files = 'params-paper1-logitnorm-newPop-full-c'

files = list.files('outputs/', pattern = params_files, full.names = TRUE)

pr_dats = c(GPCC = 'GPCC', CRU = 'CRU', MSWEP = 'MSWEP', CMORPH = 'CMORPH')
dr_vars = c(MADD = 'MADD', MADM = 'MADM', MConc = 'MConc', MDDM = 'MDDM')
ba_dats = c(GFED4s = 'GFED_four_s', GFED4 = 'GFED_four', MCD45 = 'MCD_forty_five', 
            MERIS = 'meris', Fire_CCI = 'MODIS')
bins = seq(0.5, 3, length.out = 1001)

cols = rbind(c("black", "#ff0000"),
             c("#0000ff", "#ffff00"))
open_dr <- function(drv) {
    files = files[grepl(drv, files)]
    openDataset <- function(ba, pr) {
        files = files[grepl(pr, files) & grepl(ba, files)]
        grabError <- function(file) 
            read.csv(file)[,'sigma'] 
            
        dats = sapply(files, grabError)
        return(dats)
       # return(hist(dats, breaks = bins, plot = FALSE)$density)
    }
    dats = lapply(pr_dats, function(i) lapply(ba_dats, openDataset, i))
    mn = mean(unlist(dats))
    out  = lapply(dats, lapply, function(i) hist(i, breaks = bins, plot = FALSE)$density)
    return(list(mn, out))
}

#dats = dats0 = lapply(dr_vars, open_dr)
#mns = sapply(dats, function(i) i[[1]])
#dats = lapply(dats, function(i) i[[2]])
mxs = mxs0 = rep(0, length(dats[[1]][[1]][[1]]))
for (dat in dats) {
    mxsi = mxs0
    for (di in dat) for (i in di) mxsi = mxsi + i#mxs[i>mxs] = i[i>mxs]
    mxs[mxsi > mxs] = mxsi[mxsi > mxs]
}

x = bins[-1] - diff(bins)
nrow = length(dats[[1]])
ncol = length(dats[[1]][[1]])
cols = apply(cols, 1, make_col_vector, limits = 1:ncol)
cols = apply(cols, 1, make_col_vector, limits = 1:nrow)

plot_dr <- function(dats, nm, mn, xaxs) {
    plot(range(bins[mxs>0]), c(0, max(mxs)), type = 'n', xaxt = 'n', yaxt = 'n',
         xlab = '', ylab = '')    
    if (xaxs) axis(1)
    mtext(nm, side = 3, line = -1.2, adj = 0.1)
    y = rep(0, length(dats[[1]][[1]]))
    for (i in 1:nrow) for (j in 1:ncol) {
        y0 = y
        y = y + dats[[i]][[j]]
        polygon(c(x, rev(x)), c(y, rev(y0)),
                col =  make.transparent(cols[i,j], 0.0), border = cols[i,j])
    }
    
    #for (i in 1:nrow) for (j in 1:ncol)
    #    polygon(x, dats[[i]][[j]], col = make.transparent(cols[i,j], 0.95), border = cols[i,j])
    #for (i in nrow:1) for (j in ncol:1)
    #    polygon(x, dats[[i]][[j]], col = make.transparent(cols[i,j], 0.95), border = cols[i,j])
    mtext.units(side = 3, adj = 0.9, paste('~mu~:', round(mn, 4)), lin = -1.5)
    grid()
}
png("figs/erro_dist.png", res= 300, units = 'in', height = 7.2, width = 7.2)
    par(mfrow = c(2, 2), mar = rep(0.5, 4), oma = c(3, 0, 0, 0)) 
    mapply(plot_dr, dats, names(dr_vars), mns, xaxs = c(F, F, T, T))
    mtext.units(side = 1, line = 2, '~sigma~', outer = TRUE)

    xy = par("usr")
    x = diff(xy[1:2]) * seq(0.4, 0.63, length.out = ncol) + xy[1]
    y = diff(xy[3:4]) * seq(0.4, 0.63, length.out = nrow) + xy[3]
    inds = matrix(1:(ncol * nrow), ncol = ncol)

    image(x = x, y = y, z = t(inds), add = TRUE, col = as.vector(cols))
    text(x = x, y = max(y) + diff(y[1:2])/1.5, names(ba_dats), srt = 45, adj = 0)
    text(x = max(x) + diff(x[1:2])/1.5, y = y, names(pr_dats), srt = 45, adj = 0)
dev.off()

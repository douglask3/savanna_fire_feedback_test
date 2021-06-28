

legend4way <- function(limits, cols, clabs, labdOrder = c(1:3), y2xR = 0.3, lbRt = 45) {
    nl = length(limits) + 1
    di = 1/(2*(nl-1))
    plot(c(0, 9), c(0, 9*y2xR), xlab = '', ylab = '', type = 'n', axes = FALSE)

    addBlock <- function(x, y, col, txt = '', dx = di, dy = di, txtCol = 'black', ...) {
        polygon(x + dx * c(-1, 1, 1, -1, -1), y + dy * c(-1, -1, 1, 1, -1), col = col, ...)
        
        text(txt, x = x, y = y, col = txtCol)
    }

    xs = ys = zs = xs0 = ys0 = seq(0, 1, length.out = nl)
    xs = rep(xs, nl)
    ys = rep(ys, each = nl)

    xi = xs*(nl-1)+1; yi = ys * (nl-1)+1
    
    mkTxt <- function(a, b, c) {
        labs = cbind(clabs[[2]][a], clabs[[3]][b], clabs[[1]][c])
        labs = labs[,labdOrder]
        rev(apply(labs, 1, paste, collapse = ''))
    }

    addBlocks <- function(offx, offy, acol, atxt) {
        colss = paste0('#', cols[acol], cols[yi], cols[xi])
        txt = mkTxt(xi, atxt, yi)
        mapply(addBlock, xs+offx, ys+offy, colss, txt)
        
        text(x = xs0[-1] - diff(xs0)/2 + offx, y=offy-di, round(limits,2) , adj = c(0.5, 2))
        text(x = offx-di, ys0[-1] - diff(ys0)/2 + offy, round(limits,2), adj = c(1.5, 0.5))
    }   
    addBlocks(1, 1, 1, 3)
    addBlocks(3.5, 1, 2, 2)
    addBlocks(6, 1, 3, 1)
    
    
    colss = paste0('#', cols[2], cols[yi], cols[xi])
    txt = mkTxt(xi, 2, yi)
    mapply(addBlock, xs+3.5, ys+1, colss, txt)

    colss = paste0('#', cols[3], cols[yi], cols[xi])
    txt = mkTxt(xi, 1, yi)
    mapply(addBlock, xs+6, ys+1, colss, txt, txtCol = rev(c('white', 'white', 'black', 'white', 'white', 'black', 'white', 'black', 'black')))



    text('Not\nlimited', x = 0.5, y = 0.5, srt = -lbRt, xpd = NA)
    text('Stress/Exclude\nco-limited', x = 2.5, y = 2.5, srt = -lbRt, xpd = NA)
    text('Stress\nlimited', x = 0.5, y = 2.5, srt = lbRt, xpd = NA)
    text('Exclude\nlimited', x = 2.5, y = 0.5, srt = lbRt, xpd = NA)


    text('MAP\nlimited', x = 5.5, y = 0.5, srt = -lbRt, xpd = NA)
    text('All\nlimited', x = 7.5, y = 2.5, srt = -lbRt, xpd = NA)
    text('Stress/MAP\nco-limited', x = 5.5, y = 2.5, srt = lbRt, xpd = NA)
    text('Exclude/MAP\nco-limited', x = 7.5, y = 0.5, srt = lbRt, xpd = NA)
    #colss = paste0('#', cols[xi], cols[1], cols[yi])
    #mapply(addBlock, xs+3, ys+3, colss)

    points(rep(8.5, 3), zs+1, pch = 19, cex = seq(0, 1, 0.5))
    text(x = rep(8.6, 3), y = zs+1, rev(col4Labs[[4]]), adj = 0, xpd = NA)
}


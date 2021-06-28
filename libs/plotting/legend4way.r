

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

    #cols = rev(cols)
    #test = (xs + ys) <=1
    #xs = xs[test]; ys = ys[test]
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
    #colss = paste0('#', cols[xi], cols[2], cols[yi])
    #mapply(addBlock, rev(xs)+1.25, ys+3, colss)

    #colss = paste0('#', cols[xi], cols[3], cols[yi])
    #mapply(addBlock, rev(xs)+1.25, rev(ys)+4.75, colss)
    
    #colss = paste0('#', cols[xi], '00', cols[yi])
    #mapply(addBlock, xs, rev(ys)+1, colss)

    #colss = paste0('#', '00', cols[xi], cols[yi])
    #mapply(addBlock, rev(ys) + 1, xs, colss)
}














if (F) {
nl = length(limits) + 1
di = 1/(2*(nl-1))
plot(c(-5, 5), c(-5, 5), xlab = '', ylab = '', type = 'n')

a = b = c = 0:1
abc = cbind(a, rep(b, each = 2), rep(c, each = 4), NaN, NaN, NaN)
abc[,4]  = apply(abc, 1, sum, na.rm = TRUE)

#abc = abc[apply(abc, 1, min, na.rm = TRUE) ==0,]
#abc = abc[apply(abc, 1, max, na.rm = TRUE) <6,]
abc = abc[!apply(abc[,1:3], 1, function(i) all(i == c(1,1,1))),]
abc = abc[!apply(abc[,1:3], 1, function(i) all(i == c(2,2,2))),]
mina = apply(abc, 1, min, na.rm = TRUE)
calXY <- function(abc, i, j, s) {
    #browser()
    test = abc[,i] == mina
    ang =  2*pi*(1+0.25+s+abc[test,j]/apply(abc[test,-c(i, 4:6)],1, sum))/3
    
    abc[test, 5] = cos(ang) * abc[test,4]; abc[test, 6] = sin(ang) * abc[test,4]
    return(abc)
}

for (i in list(c(3, 2, 0), c(1, 3, 1), c(2, 1, 2)))
    abc = calXY(abc, i[1], i[2], i[3])
abc[1, 5:6] = 0
cols = rev(cols)
colsl = paste0('#', cols[abc[,1]+1], cols[abc[,2]+1], cols[abc[,3]+1])

plot(abc[,5], abc[,6], pch = 19, cex = 30, col = colsl, xlim = c(-10, 10), ylim = c(-10, 10))
for (cex in seq(30, 0, length.out = 100))
    points(abc[,5], abc[,6], pch = 19, cex = cex, col = colsl)

x = seq(0, 2, by = 0.01)
y = 2 - x
s=0
ang = pi/2 + 2*pi*(-0.25 + x/(x + y))/3

x = c(cos(ang),cos(ang + 2*pi/3), cos(ang + 4*pi/3))
y = c(sin(ang),sin(ang + 2*pi/3), sin(ang + 4*pi/3))
polygon(c(x*6, 5*rev(x)), c(y*6, 5*rev(y)), col = '#888888', border = NA)
polygon(c(x*10, 6*rev(x)), c(y*10, 6*rev(y)), col = 'white', border = NA)
lines(x*7, y*7)
#lines(c(0, abc[
browser()
}

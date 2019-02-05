linear.bounded <- function(x, a, b, minY = 0, maxY = 1) {
	y = a * x + b 
	y[y>maxY] = maxY
	y[y<minY] = minY
	return(y)
}
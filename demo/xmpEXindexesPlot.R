
#
# Example:
#	Extremal Indexes Plot
#
# Description:
#  	Plot the extremal; index for a long(80'000 points) 
#	and a short (8'000 points) series of Student-t 
#	distributed (4 degress of freedom) random variables 
#	together with their EMA smoothed (lambda=0.2) series. 
#  	Compare the result with the extremal index obtained 
#	from the BMW stock and NYSE Composite Index data.
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Settings:

	par(mfrow = c(4, 2), cex = 0.5)
	set.seed(773)
	data(bmwres)
	data(nyseres)
   	blocklength = 500
	
   	
# Investigate Large Sample Random Variables:

  	x = rt(n = 80000, df = 4)
   	exindexesPlot(x, blocklength)
   	mtext("Student - 80000 points", line = 0.5, cex = 0.5)
   	lines(x = c(0.990, 0.999), y = c(1, 1), col = "red")

   	
# Investigate EMA-Smoothed rvs:

	lambda = 0.2
   	xlam = x * lambda
 	xlam[1] = x[1]
	x = filter(xlam, filter = (1-lambda), method = "rec")
   	exindexesPlot(x, blocklength)
   	mtext("EMA Student - 80000 points", line = 0.5, cex = 0.5)
   	lines(x = c(0.990, 0.999), y = c(1, 1), col = "red")

   	
# Investigate Small Sample Random Variables:

  	x = rt(n = 8000, df = 4)
   	exindexesPlot(x, blocklength)
   	mtext("Student - 8000 points", line = 0.5, cex = 0.5)
   	lines(x = c(0.990, 0.999), y = c(1, 1), col = "red")

   	
# Investigate EMA-Smoothed rvs:

	lambda = 0.2
   	xlam = x * lambda
 	xlam[1] = x[1]
	x = filter(xlam, filter = (1-lambda), method = "rec")
   	exindexesPlot(x, blocklength)
   	mtext("EMA Student - 8000 points", line = 0.5, cex = 0.5)
   	lines(x = c(0.990, 0.999), y = c(1, 1), col = "red")
		
   	
# Investigate BMW log-Returns:

  	x = bmwres[, 1]
  	length(x) 
	exindexesPlot(x, blocklength)
	mtext("BMW - 6146 points - upper", line = 0.5, cex = 0.5)
	lines(x = c(0.990, 0.999), y = c(1, 1), col = "red")
	exindexesPlot(-x, blocklength)
	mtext("BMW - 6146 points - lower", line = 0.5, cex = 0.5)
	lines(x = c(0.990, 0.999), y = c(1, 1), col = "red")
	
	
# Investigate NYSE log-Returns:

  	x = nyseres[, 1]
   	length(x)
	exindexesPlot( x, blocklength)
	mtext("NYSE - 8390 points - upper", line = 0.5, cex = 0.5)
	lines(x=c(0.990, 0.999), y = c(1, 1), col = "red")
  	exindexesPlot(-x, blocklength) 
  	mtext("NYSE - 8390 points - lower", line = 0.5, cex = 0.5)
  	lines(x = c(0.990, 0.999), y = c(1, 1), col = "red")
 

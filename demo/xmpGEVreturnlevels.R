
#
# Example:
#	Return Levels
#
# Description:
#   Plot the return levels for the GEV
#   for different values of xi.
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Settings:

	par(mfrow = c(1, 1), cex = 1)	
	p = seq(0.001, 0.999, length = 500)
	x = -1/log(1-p)

		
# Plot Return Levels:

	plot (x, qgev(1-p, xi = +0.30) , type = "l", log = "x",
	  xlab = "-1/log(1-p)", ylab = "Return level", 
	  main = "Return Levels", col = "steelblue3")
	lines(x, qgev(1-p, xi = +0.15), col = "steelblue3")
	lines(x, qgev(1-p, xi = 0.00), lwd = 2)
	lines(x, qgev(1-p, xi = -0.15), col = "steelblue3")
	lines(x, qgev(1-p, xi = -0.30), col = "steelblue3")

		
# Place Text:

	text(450,  1.3, "-0.30", cex = 0.6)
	text(450,  5.4, "-0.15", cex = 0.6)
	text(450,  7.5, " 0"   , cex = 0.6)
	text(450, 12.0, " 0.15", cex = 0.6)
	text(450, 21.0, " 0.30", cex = 0.6)

	
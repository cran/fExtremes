
#
# Example:
#	POT Parameter Estimation
#
# Description:
#   Estimate the parameters (xi, mu, sigma) for a data vector
#   x from the point process over a threshold u using the
#   function ppFit().  
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Settings:

 	par(mfcol = c(3, 2), cex = 0.6)
 	data(nyseres)
 	data = nyseres[, 1]

 	
# NYSE Residuals:

	plot(data, type = "l", ylim = c(-0.22, +0.22), main = "log Returns")
		
	
# Point Process of Threshold Exceedences:  

	u = 0.02
	y = data[data < -u]
	x = (1:length(data))[data < -u]
	points(x, y, col = 5)
		
	
# Point Process Fit:

	fit = ppFit(x = -data, threshold = u, nobs = 252)
	print(fit)
	summary(fit)


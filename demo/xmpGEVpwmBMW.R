
#
# Example:
#	GEV Probability Weighted Moments Fit
#
# Description:
#   Estimate the parameters for the GEV applying
#   the method of probability weighted moments.
#   Use quarterly (63 days) blocklengths. 
#   Plot the log returns, the block maxima of the
#   lower tail, the block maxima histogram and the
#   estimated GEV / empirical data QQ plot.
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Settings:

	par(mfrow = c(3, 2), cex = 0.6)
	data(bmwres)
	bmwres = bmwres[, 1]
	blocklength = 63

	
# Plot Time Series Data:

	plot(bmwres, type = "h", main = "Daily log Returns")

# Create Block Maxima of Lower Tail:

	x = blockMaxima(-bmwres, block = blocklength)
	mtext("Block Maxima - Lower Tail", line = 0.5, cex = 0.5)

# PWM Estimate:	

  	fit = gevFit(x, type = "pwm")
  	xi = fit$par.ests[1]
  	sigma = fit$par.ests[2]
  	mu = fit$par.ests[3]
  	
# Histogram Plot and GEV Density:

	hist(x, nclass = 20, probability = TRUE, col = "steelblue3", 
		border = "white", main = "Block Maxima - Histogram")
	s = seq(0, max(x), length = 500)
	lines(s, dgev(s, xi, mu, sigma), col = 1)
	mtext("Line: GEV Fit", line = 0.5, cex = 0.5)
		
# QQ-Plot:

	plot(sort(x), qgev(ppoints(x), xi, mu, sigma), 
	  	main="QQ-Plot: Empirical / GEV", 
	  	xlab = "empirical rvs", ylab = "GEV df")
	lines(c(min(x), max(x)), c(min(x), max(x)), col = "steelblue3")

	

#
# Example:
#	GEV Probability Weighted Moments Fit
#
# Description:
# 	Estimate the parameters of a simulated GEV 
#	with the method of probability weighted 
#	moments. 
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Settings:

	par(mfrow = c(2, 2), cex = 0.6)
  	n = 8000
	xmax.density = 15
	parm0 = list(xi = 1/4, mu = 0.0, sigma = 1.0)

	
# Create and Plot the Random Variables:

	x = rgev(n, xi = parm0$xi, mu = parm0$mu, sigma = parm0$sigma)
	plot(x, type = "h", main = "Random Variables")
	mtext("Simulated GEV Data", line = 0.5, cex = 0.5)
	lines(x = c(0, length(x)), y = c(0, 0), col = "steelblue3")
	lines(x = c(0, length(x)), y = c(-1/0.3, -1/0.3), col = "steelblue3")

	
# PWM Estimate:  
	
  	parm = gevFit(x, type = "pwm")
	parm

	
# Plot Empirical and Estimated Densities:

    d = density(x, n = 200)   
	plot(d$x, d$y, xlim = c(-5, 15), ylim=c(0, 0.4),
		xlab = "x", ylab = "density", main = "GEV Density")		
	mtext("Simulated GEV Data", line = 0.5, cex = 0.5)
	s = seq(-5, 15, length = 200)
	lines(s, dgev(s, xi = parm0$xi, mu = parm0$mu, 
		sigma = parm0$sigma), col = "steelblue3")

	

#
# Example:
#   Mean Excess Function Plot
#
# Description:
#	Plot the mean excess functions for randomly 
#	distributed residuals according to 
#   1) an exponential distribution function,
#   2) a lognormal distribution function,
#   3) an alpha-stable distribution function.
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Settings:

	par(mfrow = c(2, 2), cex = 0.6)	
	n = 10000

		
# Mean Excess Function Plot:
	
	set.seed(7138)
	mxfPlot(rexp(n, rate = 2), tail = 1, labels = FALSE)
	title(xlab = "Threshold: u", ylab = "Mean Excess: e",
		main = "Exponential DF")
	abline(0.5, 0)
	set.seed(6952)
	mxfPlot(rlnorm(n, meanlog = 0, sdlog = 2), tail = 1,
		xlim = c(0, 90), ylim = c(0, 150), labels = FALSE)
	title(xlab = "Threshold: u", ylab = "Mean Excess: e",
		main = "Lognormal DF")
	set.seed(9835)
	mxfPlot(rsymstb(n, alpha = 1.7), tail = 0.1,
		xlim = c(0, 10), ylim = c(0, 6), labels = FALSE)
	title(xlab = "Threshold: u", ylab = "Mean Excess: e",
		main = "1.7 stable DF")
	abline(0, 0.7)

	
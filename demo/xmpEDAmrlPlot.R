
#
# Example - Mean Residual Life Function Plot
#
#   Compare the mean residual life plots on the same
#   scale for six different sets with 5000 data points
#   of alpha stable random variables with index 1.7. 
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Settings:

	par(mfrow = c(3, 2), cex = 0.6)	
	seed = c(998, 745, 341, 145, 555, 876) 	
	mains = c("MRL Plot - Sample 1", "MRL Plot - Sample 2", 
	          "MRL Plot - Sample 3", "MRL Plot - Sample 4", 
	          "MRL Plot - Sample 5", "MRL Plot - Sample 6") 
	n = 5000

			
# Mean Residual Life Plot:

	for (i in 1:6) {
		set.seed(seed[i])
		mrlPlot(rsymstb(n, alpha = 1.7), nint = 100, 
			labels = FALSE, plottype = "", xlim = c(0, 60), 
			ylim = c(-50, 150))
		title(xlab = "u", ylab = "e", main = mains[i])			
	}


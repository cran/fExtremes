
#
# Example:
# 	Records Plot
#
# Description:
#  	Plot the records of a normal rvs series
#   Plot of Record Development
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Settings:

	par(mfrow = c(1, 1))
	x = rnorm(50000)
	
	
# Plot Records:

	ans = recordsPlot(x)
	print(ans)
	


#
# Example:
#	Shape Parameter Plot 
#
# Description:
#  Plot the shape parameter obtained from MDA estimators
#   for the NYSE Composite Index log returns. 
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Settings:

	par(mfcol = c(3, 2), err = -1, cex = 0.6)
	data(nyseres)
		 
# Chart Parameters:

  	tails = c(  0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.10)
	doplot = c(FALSE,FALSE,FALSE,FALSE, TRUE,FALSE,FALSE,FALSE,FALSE,FALSE)

# Calculate and Plot Shape Parameters:

	s = shaparmPlot(x = nyseres, tails = tails, 
		doplot = doplot, doprint = TRUE, xi.range = c(-1, 3), 
		alpha.range = c(0, 8))

		
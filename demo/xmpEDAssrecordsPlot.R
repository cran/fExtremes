
#
# Example:
#	Subsample Records Plot
#
# Description:
#  	Plot the records, either on a linear or logarithmic
#  	of a data set and calculate the number of recurds. 
#  	The data set can be cutted in individual subsamples.
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Settings:

	par(mfcol = c(3, 2), cex = 0.6)
	data(bmwres)
	data(nyseres)
	stable = rsymstb(n = 8000, alpha = 1.7)
	
	
# Plot Records on log-Scale:

	ssrecordsPlot(stable, subsamples = 8, plottype = "log")
		mtext("1.7 - stable Returns", line = 0.5, cex = 0.6)
	ssrecordsPlot(bmwres, subsamples = 6, plottype = "log")
		mtext("BMW Returns", line = 0.5, cex = 0.6)
	ssrecordsPlot(nyseres, subsamples = 8, plottype = "log")
		mtext("NYSE Returns", line = 0.5, cex = 0.6)

	
# Plot Records on lin-Scale:

	ssrecordsPlot(stable, subsamples = 8, plottype = "lin") 
		mtext("1.7-stable Returns", line = 0.5, cex = 0.6)
	ssrecordsPlot(bmwres, subsamples = 6, plottype = "lin") 
		mtext("BMW Returns", line = 0.5, cex = 0.6)
	ssrecordsPlot(nyseres, subsamples = 8, plottype = "lin") 
		mtext("NYSE Returns", line = 0.5, cex = 0.6)
	

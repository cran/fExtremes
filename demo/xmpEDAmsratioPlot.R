
#
# Example:
#   Ratio of Maximum and Sum Plot
#
# Description:
#	Plot the ratio of maximum and sum for normal,
#	exponential, Student-t, Cauchchy rvs. Compare
#	the result to empiral data, i.e. to the log
#	returns of the BMW stocks and the NYSE Composite
#	index.
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Settings:

	par (mfrow = c(3, 2), cex = 0.6)
	data(bmwres)
	data(nyseres)
	set.seed(876)
	nt = 10000
	x0 = rnorm(nt)
	x1 = rexp(nt)
	x2 = rt(nt, df= 4)
	x3 = rsymstb(nt, 1.0)  
	x4 = bmwres
	x5 = nyseres
	
	
# Calculate Ratio of Maximum and Sum:

	msratioPlot(x0, labels = FALSE)
	title(xlab = "Trials", ylab = "Records", main = "Standard Normal")
	msratioPlot(x1, labels = FALSE)
	title(xlab = "Trials", ylab = "Records", main = "Exponential")
	msratioPlot(x2, labels = FALSE)
	title(xlab = "Trials", ylab = "Records", main = "Student-t")
	msratioPlot(x3, labels = FALSE)
	title(xlab = "Trials", ylab = "Records", main = "Cauchy")
	msratioPlot(x4, labels = FALSE)
	title(xlab = "Trials", ylab = "Records", main = "BMW Returns")
	msratioPlot(x5, labels = FALSE)
	title(xlab = "Trials", ylab = "Records", main = "NYSE Returns")

	

#
# Example:
#	POT Point Process
#
# Description:
#	Plot the point Process p(n) for n = 5, 10,
#	100, 500, 1000, 10000 respectivel with the
#	X(i) exponentially distributed
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Settings:

	par(mfrow = c(3, 2), cex = 0.6)	
	x = seq(-2, 4, length = 200)
	set.seed(671)
	
	
# Point Process:

	an = function(n) {1}
  	bn = function(n) {log(n)}
	n = c(5, 10, 100, 500, 1000, 10000)
	titles = c("n = 5", "n = 10", "n = 100", "n = 500", 
		"n = 1000", "n = 10000")
	x = rexp(n[length(n)])

	
# Graphs:

	for ( i in 1:length(n) ) {
		plot( (1:n[i])/(n[i]+1), (x[1:n[i]]-bn(n[i]))/an(n[i]),
			xlab = "x", ylab = "y", ylim = c(-10, 3), 
			main = titles[i])
		print(bn(n[i])) }		

	
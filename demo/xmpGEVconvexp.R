
#
# Example:
#	Convergence of Exponential Maxima
#
# Description:
#  	Demonstrate the convergence of exponential
#   maxima to the Gumbel limit.
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Settings:

	par(mfrow = c(2, 1))	
	x = seq(-2, 4, length = 200)
	
# Convergence of Exponential Distribution:

	pgumbel = function(x) { pevd(x, loc = 0, scale = 1, shape = 0) }
	an = function(n) {1}
  	bn = function(n) {log(n)}
	
# Plot Convergence:

	plot(x, pgumbel(x), lwd = 2, type = "l",
	  	main = "Convergence of Exp Maxima")
	for ( n in c(10, 5, 3, 2, 1) ) {
	  	y = ( pexp( an(n)*x+bn(n) ) )^n
	  	lines(x, y, col = "steelblue3") }
	  		
# Alternative Plot:

	plot(-log(-log(pgumbel(x))), x, lwd = 2, type = "l",
	  	main = "Convergence of Exp Maxima")
	for ( n in c(10, 5, 3, 2, 1) ) {
	  	y = ( pexp( an(n)*x+bn(n) ) )^n
	  	s = -log(-log(y))
	  	lines(s[s > -2], x[s > -2], col = "steelblue3") }
	

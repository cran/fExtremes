
#
# Example:
#	Convergence of Normal Maxima
#
# Description:
#  	Demonstrate the convergence of normal
#	maxima to the Gumbel limit.
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Settings:

	par(mfrow = c(2, 1))	
	x = seq(-2, 4, length = 200)
	
# Convergence of Normal Distribution:
	pgumbel = function(x) { pevd(x, loc = 0, scale = 1, shape = 0) }
	an = function(n) { 1/sqrt(2*log(n)) }
	bn = function(n) { sqrt(2*log(n)) - 
		( log(log(n))+log(4*pi) ) / sqrt(2*log(n)) /2 }
	
# Plot Convergence:	

	x = seq(-2, 5, length = 500)
	plot(x, pgumbel(x), lwd = 2, type = "l",
	  	main = "Convergence of Gaussian Maxima")
	for ( n in c(100, 50, 10, 5, 2) ) {
	  	y = ( pnorm( an(n)*x+bn(n) ) )^n
	  	lines(x, y, col = "steelblue3") }
	  		
# Alternative Plot:

	plot(-log(-log(pgumbel(x))), x, xlim = c(-2, 12), lwd = 2, 
		type = "l", main = "Convergence of Gaussian Maxima")
	x = seq(-2, 12, length = 500)
	for ( n in c(100, 50, 10, 5, 2) ) {
	  	y = ( pnorm( an(n)*x+bn(n) ) )^n
	  	s = -log(-log(y))
	  	lines(s[x<5], x[x<5], col = "steelblue3") }	
		
		
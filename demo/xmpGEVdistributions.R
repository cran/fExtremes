
#
# Example:
#	GEV Distributions
#
# Description:
#   Plot the probability and density for the Weibull,
#   Frechet and Gumbel distributions.
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Settings:

	par(mfrow = c(2, 1))	
	s = seq(1.e-5, +6, length = 100)

# Distributions:	
	
	dweibl = function (x, alpha) {# x < 0, alpha > 0		
		alpha*((-x)^(alpha-1))*exp(-(-x)^alpha) }
	pweibl = function (q, alpha) {# q < 0, alpha > 0
		exp(-(-q)^alpha) }
	qweibl = function (p, alpha) {# alpha > 0
		-(-log(p))^(1/alpha) }
	rweibl = function (n, alpha) {# alpha > 0
		-(-log(runif(n)))^(1/alpha) }

	dgumbel = function (x) {# x real
	 	exp(-exp(-x))*exp(-x) }
	pgumbel = function (q) {# q real
	   	exp(-exp(-q)) }
	qgumbel = function (p) {	
		-log(-log(p)) }
	rgumbel = function (n) {	
		-log(-log(runif(n))) }

	dfrechet = function (x, alpha) {# x > 0, alpha > 0
	  	alpha*(x^(-alpha-1))*exp(-x^(-alpha)) }
	pfrechet = function (q, alpha) {# x  >0, alpha > 0  		
		exp(-q^(-alpha))}
	qfrechet = function (p, alpha) {# abs() handles Inf from q=1
		abs((-log(p))^(-1/alpha)) }
	rfrechet = function (n, alpha) {
		(-log(runif(n)))^(-1/alpha)	}
	
# Probability:	

	plot(x = c(-6, 6), y = c(0, 1), type = "n", 
	  	xlab = "x", ylab = "probability", main = "Probability")
	lines(x = c(-rev(s), 6), y = c(pweibl(-rev(s), alpha = 1), 1), 	
		col = 3, lty = 2)
	lines(x = c(-rev(s), s), y = c(pgumbel(-rev(s)), pgumbel(s)), 
		col = 4, lty = 1)
	lines(x = c(-6, s), y=c(0, pfrechet(s, alpha = 1)), 
		col = 2, lty = 4)
	
# Density:

	plot(x = c(-6, 6), y = c(0, 1), type = "n", 
	  	xlab = "x", ylab = "density", main = "Density")
	lines(x = c(-rev(s), 0, 6), y = c(dweibl(-rev(s), alpha = 1), 0, 0), 
		col = 3, lty = 2)
	lines(x = c(-rev(s), s), y=dgumbel(c(-rev(s), s)), 
		col = 4, lty = 1)
	lines(x = c(-6, s), y = c(0, dfrechet(s, alpha = 1)), 
		col = 2, lty = 4)
	

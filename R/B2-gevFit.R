
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA  02111-1307  USA

# Copyrights (C) 
# this R-port: 
#   by Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
# for the code accessed (or partly included) from other R-ports:
#   R: see R's copyright and license file
#   evir: original S functions (EVIS) by Alexander McNeil <mcneil@math.ethz.ch>
#     R port by Alec Stephenson <a.stephenson@lancaster.ac.uk>
#   ismev: Original S functions by Stuart Coles <Stuart.Coles@bristol.ac.uk>
#     R port/documentation by Alec Stephenson <a.stephenson@lancaster.ac.uk>
#   evd: Alec Stephenson <alec_stephenson@hotmail.com>


# ##############################################################################
# FUNCTION:				   GEV MODELLING FROM EVIS:
#  gevSim					Simulates GEV including Gumbel rvs [EVIS/EVIR]
#  gevFit					Fits GEV Distribution
#   print.gevFit 		  	 Print Method for object of class "gevFit"
#   plot.gevFit 		  	 Plot Method for object of class "gevFit"
#   summary.gevFit       	 Summary Method for object of class "gevFit"
# FUNCTION:                ADDITIONAL PLOT:
#  gevrlevelPlot     		Calculates Return Levels Based on GEV Fit
################################################################################


gevSim = 
function(model = list(shape = 0.25, location = 0, scale = 1), n = 1000)
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	Generates random variates from a GEV distribution
	
	# FUNCTION:
	
	# Simulate:
	ans = rgev(n = n, xi = model$shape, mu = model$location, 
		sigma = model$scale)
		
	# Return Value:
	ans	
}


# ------------------------------------------------------------------------------


gevFit =
function(x, block = NA, type = c("mle", "pwm"), gumbel = FALSE, ...)
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	Fits parameters to a GEV distribution
	
	# Note:
	#	Argument named "method is already used for the selection
	#	of the MLE optimization algorithm, therfore we use here
	#	"type".
	
	# FUNCTION:
	
	# Settings:
	call = match.call()
	type = type[1]
	
	# Internal Function:
	gev.pwm = function(data, block = NA, ...) {
	# Probability Weighted Moment method.
    # Blocks and data:
    n.all = NA
    if (!is.na(block)) {
        n.all = length(data)
        if (is.character(block)) {
            times = as.POSIXlt(attributes(data)$times)
            if (block %in% c("semester", "quarter")) {
                sem = quart = times$mon
                sem[sem %in% 0:5] = quart[quart %in% 0:2] = 0
                sem[sem %in% 6:11] = quart[quart %in% 3:5] = 1
                quart[quart %in% 6:8] = 2
                quart[quart %in% 9:11] = 3 }
            grouping = switch(block, 
            	semester = paste(times$year, sem), 
            	quarter = paste(times$year, quart), 
            	month = paste(times$year, times$mon), 
            	year = times$year, 
            	stop("unknown time period"))
            data = tapply(data, grouping, max) }
        else {
            data = as.numeric(data)
            nblocks = (length(data)%/%block) + 1
            grouping = rep(1:nblocks, rep(block, nblocks))[1:length(data)]
            data = tapply(data, grouping, max) } }
    data = as.numeric(data)
    n = length(data)	
	
    # Internal Function - Sample Moments:
	sampwm = function (x, nmom) {
		# a = 0, b = 0, kind = 1
		x = rev(sort(x))
		moments = rep(0, nmom)
		moments[1] = mean(x)
		n = length(x)
		for (i in 1:n) {
			weight = 1/n
			for (j in 2:nmom) {
				weight = weight*(n-i-j+2)/(n-j+1)
				moments[j] = moments[j] + weight*x[i] } }
		return(moments) }
	
	# Internal Function:
	y = function(x, w0, w1, w2) { (3^x-1)/(2^x-1) - (3*w2 - w0)/(2*w1 - w0) }		
	# Calculate:
	w = sampwm(data, nmom = 3)
	w0 = w[1]
	w1 = w[2]
	w2 = w[3]	   
	xi = uniroot(f = y, interval = c(-5,+5), 
		w0 = w[1], w1 = w[2], w2 = w[3])$root
	sigma = beta = (2*w1-w0)*xi / gamma(1-xi) / (2^xi-1)
	mu = w0 + beta*(1-gamma(1-xi))/xi
	# Output:
	fit = list(n.all = n.all, n = n, data = data, bock = block, 
		par.ests = c(xi, sigma, mu), par.ses = rep(NA, 3),
		varcov = matrix(rep(NA, 9), 3, 3), converged = NA, 
		nllh.final = NA, call=match.call(), selected = "pwm")
	names(fit$par.ests) = c("xi", "sigma", "mu")
    names(fit$par.ses) = c("xi", "sigma", "mu")	
    # Return Value:
    class(fit) = "gev" 
	fit }
	
	# Internal Function:
	gumbel.pwm = function(data, block = NA, ...) {
	# "Probability Weighted Moment" method.
	# Blocks and data:
    n.all = NA
    if (!is.na(block)) {
        n.all = length(data)
        if (is.character(block)) {
            times = as.POSIXlt(attributes(data)$times)
            if (block %in% c("semester", "quarter")) {
                sem = quart = times$mon
                sem[sem %in% 0:5] = quart[quart %in% 0:2] = 0
                sem[sem %in% 6:11] = quart[quart %in% 3:5] = 1
                quart[quart %in% 6:8] = 2
                quart[quart %in% 9:11] = 3 }
            grouping = switch(block, 
            	semester = paste(times$year, sem), 
            	quarter = paste(times$year, quart), 
            	month = paste(times$year, times$mon), 
            	year = times$year, 
            	stop("unknown time period"))
            data = tapply(data, grouping, max) }
        else {
            data = as.numeric(data)
            nblocks = (length(data)%/%block) + 1
            grouping = rep(1:nblocks, rep(block, nblocks))[1:length(data)]
            data = tapply(data, grouping, max) } }
    data = as.numeric(data)
    n = length(data)
	# Sample Moments:
    x = rev(sort(data))
	lambda = c(mean(x), 0)
	for (i in 1:n) {
		weight = (n-i)/(n-1)/n
		lambda[2] = lambda[2] + weight*x[i] } 
	# Calculate Parameters:
    xi = 0
	sigma = beta = lambda[2]/log(2)
	mu = lambda[1] - 0.5772*beta
	# Output:
	fit = list(n.all = n.all, n = n, data = data, block = block, 
		par.ests = c(sigma, mu), par.ses = rep(NA, 2),
		varcov = matrix(rep(NA, 4), 2, 2), converged = NA, 
		nllh.final = NA, call = match.call(), selected = "pwm")
	names(fit$par.ests) = c("sigma", "mu")
    names(fit$par.ses) = c("sigma", "mu")
    # Return Value:
    class(fit) = "gev" # not gumbel!
	fit }
	
	# Estimate Parameters:
	if (gumbel) {	
		# Add Call and Type
		if (length(type) > 1) type = type[1]
		# Probability Weighted Moment Estimation
		if (type == "pwm") {
			fitted = gumbel.pwm(data = x, block = block, ...) }
		# Maximum Log Likelihood Estimation
		# Use Alexander McNeils EVIS:
		if (type == "mle") { 
			fitted = gumbel(data = x, block = block, ...) } }	
	else {
		# Add Call and Type
		if (length(type) > 1) type = type[1]
		# Probability Weighted Moment Estimation:
		if (type == "pwm") { 
			fitted = gev.pwm(data = x, block = block, ...) }
		# Maximum Log Likelihood Estimation
		# Use Alexander McNeils EVIS (renames as gev.mle)
		if (type == "mle") { 
			fitted = gev(data = x, block = block, ...) }	}
			
	# Compute Residuals:
	if (gumbel) {
		# GUMBEL:
		xi = 0
		sigma = fitted$par.ests[1]
		mu = fitted$par.ests[2] 
		fitted$residuals = exp( - exp( - (fitted$data - mu)/sigma)) }
	else {
		# GEV:
		xi = fitted$par.ests[1]
		sigma = fitted$par.ests[2]
		mu = fitted$par.ests[3]
		fitted$residuals = (1 + (xi * (fitted$data - mu))/sigma)^(-1/xi) }	
		
	# Make Unique:
	fit = list()
	fit$fit = fitted
	fit$call = call
	fit$type = c(if(gumbel) "gum" else "gev", type[1])
	fit$par.ests = fitted$par.ests
	fit$par.ses = fitted$par.ses
	fit$residuals = fitted$residuals
	fit$fitted.values = fitted$data - fitted$residuals
	fit$llh = fitted$nllh.final
	fit$converged = fitted$converged
		
	# Return Value:
	class(fit) = "gevFit"
	fit
}


# ------------------------------------------------------------------------------


print.gevFit =
function(x, ...)
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	Print Method for an object of class "gevFit".
	
	# FUNCTION:
	
	# Function Call:
	cat("\nCall:\n")
	cat(paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n", sep = "")
	 		
	# Estimation Type:
	cat("\nEstimation Type:", x$type, "\n")	
	
	# Estimated Parameters:
	cat("\nEstimated Parameters:\n")
	print(x$par.ests)
	cat("\n")
	
	# Return Value:
	invisible(x)
}


# ------------------------------------------------------------------------------


plot.gevFit =
function(x, which = "all", ...)
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	Plot method for an object of class "gevFit".
	
	# Details:
	# 	plot.gev:
	#	Data are converted to unit exponentially distributed residuals
    #	under null hypothesis that GEV fits. Two diagnostics for iid
    #	exponential data are offered:
    #	"Scatterplot of Residuals" and "QQplot of Residuals"

	# FUNCTION:	
	
	# Internal Plot Functions:
	plot.1 <<- function(x) {
		plot(x$fit$data, type = "h", 
			xlab = "Index",
			ylab = "Data", 
			main = "Block Maxima") }
	plot.2 <<- function(x) {
	    plot(x$residuals, 
	    	xlab = "Ordering",
	    	ylab = "Residuals",  
	    	main = "Scatterplot of Residuals")
	   	lines(lowess(1:length(x$residuals), x$residuals)) }
	plot.3 <<- function(x) {            
		# evir::qplot
		qplot(x$residuals, 
			# xlab = "Ordered Data",
			# ylab = "Exponential Quantiles",
	 		main = "Quantile-Quantile Plot") }
	 		
	# Plot:
	interactivePlot(
		x = x,
		choices = c(
			"Block Maxima Plot", 
			"Scatterplot of Residuals", 
			"Quantile Quantile Plot"),
		plotFUN = c(
			"plot.1", 
			"plot.2", 
			"plot.3"),
		which = which)
	            	
	# Return Value:
	invisible(x)
}


# ------------------------------------------------------------------------------
  

summary.gevFit =
function(object, doplot = TRUE, which = "all", ...) 
{
	# A function implemented by Diethelm Wuertz

	# Description:
	#	Summary method for an object of class "gevFit".

    # FUNCTION:
    
    # Print:
    print(object, ...)
	
	# Summary:
	if (object$type[2] == "mle") {
		cat("\nStandard Deviations:\n"); print(object$par.ses)
		cat("\nLog-Likelihood Value: ", object$llh)
		cat("\nType of Convergence:  ", object$converged, "\n") } 
	cat("\n")
	
	# Plot:
	if (doplot) plot(object, which = which, ...)
	cat("\n")
	
	# Return Value:
	invisible(object)
}


# ------------------------------------------------------------------------------


gevrlevelPlot =
function(object, k.blocks = 20, add = FALSE, ...)
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	Calculates Return Levels Based on GEV Fit
	
	# FUNCTION:
	
	# Settings
	fit = object
	
	# Use "rlevel.gev":
	ans = rlevel.gev(out = fit$fit, k.blocks = k.blocks, add = add, ...)
	ans = c(min = ans[1], v = ans[2], max = ans[3])
	
	# Return Value:
	ans	
}


# ******************************************************************************



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


################################################################################
# FUNCTION			DESCRIPTION:
#  findThreshold     Finds threshold values
#  blocks            Creates data blocks on vectors and time series
#  blockMaxima		 Calculates block maxima on vectors and time series
#  deCluster 		 Declusters a point process
################################################################################


findThreshold =
function(x, n = NA)
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	Finds upper thresold for a given number of Extremes.
	
	# Arguments:
	# 	n 	- a numeric value or vector giving number of extremes 
	#         above the threshold. If "n" is not specified, "n"
	#		  is set to an integer representing 5% of the data 
	#         from the whole data set "x".
		
	# Note:
	#	Imported from R-package evir/EVIS.

	# FUNCTION:
	
	# Settings:
	if(is.na(n[1])) n = floor(0.05*length(x))
	
	# Continue:
	x = rev(sort(as.numeric(x)))
	thresholds = unique(x)
	indices = match(x[n], thresholds)
	indices = pmin(indices + 1, length(thresholds))	
	
	# Return Value:
	thresholds[indices]
}


# ------------------------------------------------------------------------------


blocks =
function(x, block = "month", FUN = max)
{	# A function implemented by Diethelm Wuertz

	# Description:
	#	Creates data blocks on vectors and time series.

	# Note:
	#	Imported from R-package evir/EVIS.
	
	# FUNCTION:
	
    # Settings:
    data = x
    
    # Compute:
    n.all = length(data)
    if(is.character(block)) {
     	times = as.POSIXlt(attributes(data)$times) 
      	if(block %in% c("semester", "quarter")) {
      		sem = quart = times$mon
           	sem[sem %in% 0:5] = quart[quart %in% 0:2] = 0
           	sem[sem %in% 6:11] = quart[quart %in% 3:5] = 1
           	quart[quart %in% 6:8] = 2
          	quart[quart %in% 9:11] = 3 }
        grouping = switch(block,
          	semester = paste(times$year, sem),
           	quarter = paste(times$year, quart),
           	quarters = paste(times$year, quart),
          	month = paste(times$year, times$mon),
          	months = paste(times$year, times$mon),
          	year = times$year,
          	years = times$year,
          	stop("unknown time period"))
      	newdata = tapply(data, grouping, FUN=FUN) }
   	else {
       	data = as.numeric(data)
       	nblocks = (length(data) %/% block) + 1
       	grouping = rep(1:nblocks, rep(block, nblocks))[1:length(data)]
       	newdata = tapply(data, grouping, FUN=FUN)}
       
  	# Return Value: 
    result = newdata 
    result
}


# -----------------------------------------------------------------------------


blockMaxima = 
function(x, block = "month", details = FALSE, doplot = TRUE, ...) 
{  	# A function implemented by Diethelm Wuertz
	
	# Description:
	#	Calculates block maxima on vectors and time series.
	
	# Arguments:
	#	x 		- may be alternatively as.vector or as.ts
	#   block 	- as.numeric:   length of a block
	#			  as.character: year | semester | quarter | month
	
	# Note:
	#   Calls McNeils Splus function blocks()
	#	Output data as vector of transposed 
	#	result to get proper order of data!

	# FUNCTION:
	
	# Settings
	x = blocks(x, block)
  	
	# Plot:
	if (doplot) {
		plot(as.vector(x), type="h", ylab = "Block Maxima",  ...)
		title(main = paste(block, "- Block Maxima"))
		grid() }
  	
	# Details:
	# if details == FALSE a vector is returned, i.e details are removed!
	if (!details) x = as.vector(x[is.na(x) == FALSE])
	
	# Return Value:
	x
}


# -----------------------------------------------------------------------------


deCluster = 
function(x, run = NA, doplot = TRUE)
{	# A function implemented by Diethelm Wuertz 

	# Description:
	#	Declusters a point process
	
	# Note:
	#	Imported from R-package evir/EVIS.
	
	# FUNCTION:
	
	# Settings:
	labels = TRUE
	
	# Imported Function:
	series = x
	picture = doplot
	n = length(as.numeric(series))
    times = attributes(series)$times
    if (is.null(times)) 
        stop("`series' must have a `times' attribute")
    as.posix = is.character(times) || inherits(times, "POSIXt") || 
        inherits(times, "date") || inherits(times, "dates")
    if (as.posix) 
        gaps = as.numeric(difftime(as.POSIXlt(times)[2:n], 
        as.POSIXlt(times)[1:(n - 1)], units = "days"))
    else gaps = as.numeric(diff(times))
    longgaps = gaps > run
    if (sum(longgaps) <= 1) 
        stop("Decluster parameter too large")
    cluster = c(0, cumsum(longgaps))
    cmax = tapply(as.numeric(series), cluster, max)
    newtimes = times[match(cmax, series)]
    newseries = structure(series[match(cmax, series)], times = newtimes)
    n = length(as.numeric(newseries))
    if (as.posix) {
        newgaps = as.numeric(difftime(as.POSIXlt(newtimes)[2:n], 
            as.POSIXlt(newtimes)[1:(n - 1)], units = "days"))
        times = as.POSIXlt(times)
        newtimes = as.POSIXlt(newtimes) }
    else {
    	newgaps = as.numeric(diff(newtimes)) }
    
    # Plot:
    if (doplot) {
        # cat("Declustering picture...\n")
        # cat(paste("Data reduced from", length(as.numeric(series)), 
        #     "to", length(as.numeric(newseries)), "\n"))
        # par(mfrow = c(2, 2))
        if (labels) {
            main = "de-Clustering"
        	plot(times, series, type = "h", main = main)
        	qPlot(gaps)
        	plot(newtimes, newseries, type = "h", main = main)
        	qPlot(newgaps) }
    }
    
    # Result:
    ans = newseries

	# Return Value:
	ans
}   
  
 
# ******************************************************************************


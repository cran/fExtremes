
#*******************************************************************************
# fExtremes - A SOFTWARE COLLECTION FOR FINANCIAL ENGINEERS
# PART III: Beyond the Sample: Dealing with Extreme Values
#
# collected by Diethelm Wuertz
# Version 0.9
#*******************************************************************************


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


# Default Settings:
	
	xmpExtremes = function(prompt = "") {invisible(prompt)}


.First.lib = 
function(lib, pkg)
{ 	# A function implemted by D. Wuertz

	# Package:
	cat("\nfExtremes:  Beyond the Sample: Dealing with Extreme Values")
	
	# Requires:
	# minor = as.numeric(version$minor)
	# DEBUG = TRUE
	# sink("@sink@")           					# available from:
    # library(evd, warn.conflicts = DEBUG)		# CRANbin
	# library(ismev, warn.conflicts = DEBUG)	# CRANbin
    # library(evir, warn.conflicts = DEBUG)		# evir | Now on Cran
	# sink()
	# unlink("@sink@")
	

	
	# Load dll:
	# library.dynam("fExtremes", pkg, lib)
	
}


# ******************************************************************************



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
# FUNCTION:				GEV DISTRIBUTION FAMILY: [USE FROM EVD]
#  devd					 Density for the GEV Distribution 
#   pevd				  Probability for the GEV Distribution
#   qevd				  Quantiles for the GEV Distribution
#   revd				  Random variates for the GEV Distribution
# FUNCTION:				GEV DISTRIBUTION FAMILY: [USE FROM EVIS]
#  dgev					 Density for the GEV Distribution 
#   pgev				  Probability for the GEV Distribution
#   qgev				  Quantiles for the GEV Distribution
#   rgev				  Random variates for the GEV Distribution
################################################################################


devd = 
function (x, loc = 0, scale = 1, shape = 0, log = FALSE) 
{
    # FUNCTION:
    
    if (min(scale) <= 0) 
        stop("invalid scale")
    if (length(shape) != 1) 
        stop("invalid shape")
    x = (x - loc)/scale
    if (shape == 0) 
        d = log(1/scale) - x - exp(-x)
    else {
        nn = length(x)
        xx = 1 + shape * x
        xxpos = xx[xx > 0 | is.na(xx)]
        scale = rep(scale, length.out = nn)[xx > 0 | is.na(xx)]
        d = numeric(nn)
        d[xx > 0 | is.na(xx)] = log(1/scale) - xxpos^(-1/shape) - 
            (1/shape + 1) * log(xxpos)
        d[xx <= 0 & !is.na(xx)] = -Inf
    }
    if (!log) 
        d = exp(d)
    d
}


# ------------------------------------------------------------------------------


pevd =
function (q, loc = 0, scale = 1, shape = 0, lower.tail = TRUE) 
{
    # FUNCTION:
    
    if (min(scale) <= 0) 
        stop("invalid scale")
    if (length(shape) != 1) 
        stop("invalid shape")
    q = (q - loc)/scale
    if (shape == 0) 
        p = exp(-exp(-q))
    else p = exp(-pmax(1 + shape * q, 0)^(-1/shape))
    if (!lower.tail) 
        p = 1 - p
    p
}


# ------------------------------------------------------------------------------


qevd = 
function (p, loc = 0, scale = 1, shape = 0, lower.tail = TRUE) 
{
    # FUNCTION:
    
    if (min(p, na.rm = TRUE) <= 0 || max(p, na.rm = TRUE) >= 
        1) 
        stop("`p' must contain probabilities in (0,1)")
    if (min(scale) < 0) 
        stop("invalid scale")
    if (length(shape) != 1) 
        stop("invalid shape")
    if (!lower.tail) 
        p = 1 - p
    if (shape == 0) 
        return(loc - scale * log(-log(p)))
    else return(loc + scale * ((-log(p))^(-shape) - 1)/shape)
}


# ------------------------------------------------------------------------------


revd =
function (n, loc = 0, scale = 1, shape = 0) 
{
    # FUNCTION:
    
    if (min(scale) < 0) 
        stop("invalid scale")
    if (length(shape) != 1) 
        stop("invalid shape")
    if (shape == 0) 
        return(loc - scale * log(rexp(n)))
    else return(loc + scale * (rexp(n)^(-shape) - 1)/shape)
}


# ******************************************************************************


dgev =
function(x, xi = 1, mu = 0, sigma = 1, log = FALSE)
{	# A function implemented from evd

  	# Description:
	#   GEV Density Function
	#   Note: 1 + xi*(x-mu)/sigma > 0
	#   xi > 0 Frechet
	#   xi = 0 Gumbel
	#   xi < 0 weibl

	# FUNCTION:
	
	# Settings:
	loc = mu
	scale = sigma
	shape = xi
	
	# Density function:
	if (min(scale) <= 0) 
        stop("invalid scale")
    if (length(shape) != 1) 
        stop("invalid shape")
    x = (x - loc)/scale
    if (shape == 0) 
        d = log(1/scale) - x - exp(-x)
    else {
        nn = length(x)
        xx = 1 + shape * x
        xxpos = xx[xx > 0 | is.na(xx)]
        scale = rep(scale, length.out = nn)[xx > 0 | is.na(xx)]
        d = numeric(nn)
        d[xx > 0 | is.na(xx)] = log(1/scale) - xxpos^(-1/shape) - 
            (1/shape + 1) * log(xxpos)
        d[xx <= 0 & !is.na(xx)] = -Inf
    }
    if (!log) 
        d = exp(d)
	
	# Return Value:
	d
}


# ------------------------------------------------------------------------------


pgev =
function(q, xi = 1, mu = 0, sigma = 1, lower.tail = TRUE)
{	# A function implemented from evd
 
  	# Description:
	#   GEV Probability Function
	#   Note: 1 + xi*(x-mu)/sigma > 0
	#   xi > 0 Frechet
	#   xi = 0 Gumbel
	#   xi < 0 Weibull

	# FUNCTION:
	
	# Settings:
	loc = mu
	scale = sigma
	shape = xi
	
	# Probability function:
	if (min(scale) <= 0) 
        stop("invalid scale")
    if (length(shape) != 1) 
        stop("invalid shape")
    q = (q - loc)/scale
    if (shape == 0) 
        p = exp(-exp(-q))
    else p = exp(-pmax(1 + shape * q, 0)^(-1/shape))
    if (!lower.tail) 
        p = 1 - p
	
	# Return Value:
	p
}


# ------------------------------------------------------------------------------


qgev =
function (p, xi = 1, mu = 0, sigma = 1, lower.tail = TRUE)
{	# A function implemented from evd

	# Description:
	#   GEV Quantile Function
	#   Note: 1 + xi*(x-mu)/sigma > 0
	#   xi > 0 Frechet
	#   xi = 0 Gumbel
	#   xi < 0 Weibull

	# FUNCTION:
	
	# Settings:
	loc = mu
	scale = sigma
	shape = xi
	
	# Return Value:
	if (min(p, na.rm = TRUE) < 0 || max(p, na.rm = TRUE) > 1) 
        stop("`p' must contain probabilities in (0,1)")
    if (min(scale) < 0) 
        stop("invalid scale")
    if (length(shape) != 1) 
        stop("invalid shape")
    if (!lower.tail) 
        p = 1 - p
    if (shape == 0) 
        q = loc - scale * log(-log(p))
    else 
    	q = loc + scale * ((-log(p))^(-shape) - 1)/shape
    	
    # Return Value:
    q
}


# ------------------------------------------------------------------------------


rgev =
function (n, xi = 1, mu = 0, sigma = 1)
{	# A function implemented from evd

	# Description:
	#   GEV Random Variables
	#   Note: 1 + xi*(x-mu)/sigma > 0
	#   xi > 0 Frechet
	#   xi = 0 Gumbel
	#   xi < 0 Weibull

	# FUNCTION:
	
	# Settings:
	loc = mu
	scale = sigma
	shape = xi
	
	# Return Value:
	if (min(scale) < 0) 
        stop("invalid scale")
    if (length(shape) != 1) 
        stop("invalid shape")
    if (shape == 0) 
        r = loc - scale * log(rexp(n))
    else 
    	r = loc + scale * (rexp(n)^(-shape) - 1)/shape

	# Return Value:
	r
}
	
	
# ******************************************************************************



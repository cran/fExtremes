\name{gpdglmFit}

\alias{gpdglmFit}

\alias{print.gpdglmFit}
\alias{plot.gpdglmFit}
\alias{summary.gpdglmFit}

\alias{gpdglmprofPlot}
\alias{gpdglmprofxiPlot}


\title{Maximum-likelihood Fitting for the GPD Model}


\description{
  
	This is a collection of functions to model the Generalized Pareto 
	Distribution, GPD, by maximum likelihood approximation based 
	on \R's ISMEV package. In addition to \code{gpdFit} the parameter 
	estimation allows to include generalized linear modelling of each 
	parameter, therefore the name \code{gpdglmFit}.
	\cr
	
	The functions are:
	
	\tabular{rll}{
	1 	\tab	\code{gpdglmFit}			
		\tab	fits empirical or simulated data to the distribution, \cr 
  	2 	\tab	\code{print} 				
  		\tab	print method for a fitted GPD object of class ..., \cr
  	3 	\tab	\code{plot} 				
  		\tab	plot method for a fitted GPD object, \cr 
  	4 	\tab	\code{summary} 			
  		\tab	summary method for a fitted GPD object, \cr 	
  	5 	\tab	\code{gpdglmprofPlot} 		
  		\tab	profile log-likelihoods for return levels, \cr 
  	6 	\tab	\code{gpdglmprofxiPlot} 	
  		\tab	profile log-likelihoods for shape parameters. }
  	
}


\usage{
gpdglmFit(x, threshold = min(x), npy = 365, y = NULL, sigl = NULL,
    shl = NULL, siglink = identity, shlink = identity, show = FALSE,
    method = "Nelder-Mead", maxit = 10000, \dots)

\method{print}{gpdglmFit}(x, \dots)
\method{plot}{gpdglmFit}(x, which = "all", \dots)
\method{summary}{gpdglmFit}(object, doplot = TRUE, which = "all", \dots)

gpdglmprofPlot(fit, m, xlow, xup, conf = 0.95, nint = 100, \dots)
gpdglmprofxiPlot(fit, xlow, xup, conf = 0.95, nint = 100, \dots)
}


\arguments{

  	\item{conf}{
  		[gpdglmprof*Plot] -
	    the confidence coefficient of the plotted profile confidence 
	    interval.
	    }
	\item{doplot}{
		a logical. Should the results be plotted?
		}
	\item{fit}{
	    a fitted object either of class \code{"gpdglm"}.
	    }    
    \item{m}{
    	[gpdglmprof*Plot] -
	    the return level; i.e. the profile likelihood is for the value 
	    that is exceeded with probability 1/\code{m}.
	    }
	\item{maxit}{
		[gpdglmFit] -
	    the maximum number of iterations.
	    }
	\item{method}{
		[gpdglmFit] -
	    the optimization method; see \code{\link{optim}} for
	    details.
	    }
  	\item{nint}{
  		[gpdglmprof*Plot] -
	    the number of points at which the profile likelihood is evaluated.
	    }   
	\item{npy}{
		[gpdglmFit] -
	    the number of observations per year/block. By default 365.
	    }
  	\item{object}{
  		[summary] -
    	a fitted object of class \code{"gpdglmFit"}.
    	}
    \item{show}{
    	[gpdglmFit] -
	    a logical; if \code{TRUE} (the default), print details of
	    the fit.
	    }
  	\item{sigl, shl}{
  		[gpdglmFit] -
	    numeric vectors of integers, giving the columns
	    of \code{ydat} that contain covariates for generalized linear
	    modelling of the scale and shape parameters repectively
	    (or \code{NULL} (the default) if the corresponding parameter is
	    stationary).
	    }
  	\item{siglink, shlink}{
  		[gpdglmFit] -
	    inverse link functions for generalized
	    linear modelling of the scale and shape parameters repectively.
	    }
	\item{threshold}{
		[gpdglmFit] -
	    the threshold value; a single number or a numeric
	    vector of the same length as \code{xdat}.
	    }
  	\item{which}{
  		[plot][summary] - 
	    a vector of logicals, one for each plot, denoting which plot 
	    should be displayed. By default \code{c(TRUE, TRUE, TRUE, TRUE, 
	    TRUE)}.
	    }
	\item{x}{
	    A numeric vector of data to be fitted.
	    \cr
    	[print][plot] -
    	a fitted object of class \code{"gpdglmFit"}.
	    }
	\item{xlow, xup}{[gpdglmprof*Plot] -
	    the least and greatest value at which to evaluate the profile 
	    likelihood.
	    }
	\item{y}{
		[gpdglmFit] -
	    a matrix of covariates for generalized linear modelling
	    of the parameters (or \code{NULL} (the default) for stationary
	    fitting). The number of rows should be the same as the length
	    of \code{xdat}.
	    }
  	\item{\dots}{
  		[gpdglmFit] -
    	other control parameters for the optimization. These are passed
    	to components of the \code{control} argument of \code{optim}.
    	}  
      
}


\value{
  
	A list containing the following components. A subset of these
  	components are printed after the fit. If \code{show} is
  	\code{TRUE}, then assuming that successful convergence is
  	indicated, the components \code{nexc}, \code{nllh},
  	\code{mle}, \code{rate} and \code{se} are always printed.
  
  	\item{trans}{
    	An logical indicator for a non-stationary fit.
    	}
  	\item{model}{
   		A list with components \code{sigl} and \code{shl}.
   		}
  	\item{link}{
    	A character vector giving inverse link functions.
    	}
  	\item{threshold}{
    	The threshold, or vector of thresholds.
    	}
  	\item{nexc}{
    	The number of data points above the threshold.
    	}
  	\item{data}{
    	The data that lie above the threshold. For
    	non-stationary models, the data is standardized.
    	}
  	\item{conv}{
    	The convergence code, taken from the list returned by
    	\code{\link{optim}}. A zero indicates successful convergence.
    	}
  	\item{nllh}{
    	The negative logarithm of the likelihood evaluated at
    	the maximum likelihood estimates.
    	}
  	\item{vals}{
    	A matrix with three columns containing the maximum
    	likelihood estimates of the scale and shape parameters, and
    	the threshold, at each data point.
    	} 
  	\item{mle}{
   	 	A vector containing the maximum likelihood estimates.
   	 	}
  	\item{rate}{
	    The proportion of data points that lie above the
	    threshold.
	    }
  	\item{cov}{
    	he covariance matrix.
    	}
  	\item{se}{
    	A vector containing the standard errors.
    	}
  	\item{n}{
	    The number of data points (i.e.\ the length of
	    \code{xdat}).
	    }
  	\item{npy}{
    	The number of observations per year/block.
    	}
  	\item{xdata}{
    	The data that has been fitted.
    	} 
    
  	For stationary models four plots are produced; a probability
  	plot, a quantile plot, a return level plot and a histogram
  	of data with fitted density. For non-stationary models two plots 
  	are produced; a residual probability plot and a residual quantile 
  	plot.
    
}


\details{
  
	\bold{Simulation:}
	\cr\cr
	To simulate a GPD series use the function \code{gpdSim}.
	\cr
	
	\bold{Parameter Estimation:}
	\cr\cr
	\code{gpdglmFit} fits by the Maximum-likelihood approach the generalized 
  	extreme value distribution, including generalized linear modelling 
  	of each parameter. 
  	\cr
  
  	\bold{Methods:}
  	\cr\cr
  	\code{print.gpdglm}, \code{plot.gpdglm} and \code{summary.gpdglm} 
  	are print, plot, and summary methods for a fitted object of class 
  	\code{gpdglm}.
  	\cr
  
  	\bold{Nonstationary Models:}
  	\cr\cr
  	For non-stationary fitting it is recommended that the covariates
  	within the generalized linear models are (at least approximately)
  	centered and scaled (i.e.\ the columns of \code{ydat} should be
  	approximately centered and scaled).
  	
}


\author{
  
	Alec Stephenson for the code implemented from \R's ismev package, \cr
	Stuart Scott for the original code, and
	Diethelm Wuertz for this \R-port.
  	
}


\references{

Coles S. (2001);
	\emph{Introduction to Statistical Modelling of Extreme Values},
	Springer.  
	
}


\seealso{
  
  	\code{\link{mrlPlot}}, 
  	\code{\link{ppFit}}.
  	
}


\examples{
## Use Rain Data:
   data(rain)
   
## Fit GPD Model:
   xmpExtremes("Start: Parameter Estimation >")
   fit = gpdglmFit(x = rain, threshold = 10)
   print(fit)
   xmpExtremes("Next: Summary Report > ")
   
## Summarize Results:
   xmpExtremes("Next: Profile Likelihood >")
   par(mfrow = c(2, 2), cex = 0.75)
   summary(fit, which = "all")
   # Profile Lielihood:
   par(mfrow = c(2, 1), cex = 0.75)
   gpdglmprofPlot(fit, m = 10, xlow = 55, xup = 75)
   title(main = "Rain")
   gpdglmprofxiPlot(fit, xlow = -0.02, 0.15)
   title(main = "Rain")
}


\keyword{models}


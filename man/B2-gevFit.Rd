\name{gevFit}

\alias{gevSim}
\alias{gevFit}

\alias{print.gevFit}
\alias{plot.gevFit}
\alias{summary.gevFit}

\alias{gevrlevelPlot}


\title{Modelling GEV Distribution}


\description{
  
	This is a collection of functions to model the Generalized Extreme 
	Value, GEV, distribution based on \R's \code{evir} package. Two 
	types of approaches for parameter estimation are provided: Maximum 
	likelihood estimation and  probability weighted moment method.
	\cr
	
	The functions are:
	
	\tabular{rll}{
	1 	\tab	\code{gevSim}			
		\tab	generates data from the GEV, \cr
	2 	\tab	\code{gevFit}			
		\tab 	fits empirical or simulated data to the distribution, \cr 
	3 	\tab	\code{print} 			
		\tab	print method for a fitted GEV object, \cr
  	4 	\tab	\code{plot} 			
  		\tab	plot method for a fitted GEV object, \cr 
  	5 	\tab 	\code{summary}			
  		\tab	summary method for a fitted GEV object, \cr
  	6 	\tab	\code{gevrlevelPlot}	
  		\tab 	k-block return level with confidence intervals. }
  	 
}


\usage{
gevSim(model = list(shape = 0.25, location = 0, scale = 1), n = 1000)
gevFit(x, block = NA, type = c("mle", "pwm"), gumbel = FALSE, \dots)

\method{print}{gevFit}(x, \dots)
\method{plot}{gevFit}(x, which = "all", \dots)
\method{summary}{gevFit}(object, doplot = TRUE, which = "all", \dots)

gevrlevelPlot(object, k.blocks = 20, add = FALSE, \dots)
}


\arguments{

  	\item{add}{
  		[gevrlevelPlot] -
	    whether the return level should be added graphically to a
	    time series plot; if \code{FALSE} a graph of the profile
	    likelihood curve showing the return level and its confidence
	    interval is produced.
	    }
    \item{block}{
    	[gevFit] -
	    the block size. Only used if \code{method="mle"} is selected.
	    A numeric value is interpreted as the
	    number of data values in each successive block. All the data is
	    used, so the last block may not contain \code{block} observations.
	    If the \code{data} has a \code{times} attribute containing (in
	    an object of class \code{"POSIXct"}, or an object that can be
	    converted to that class; see \code{\link{as.POSIXct}}) the
	    times/dates of each observation, then \code{block} may instead
	    take the character values \code{"month"}, \code{"quarter"},
	    \code{"semester"} or \code{"year"}.
	    }
  	\item{doplot}{
		a logical. Should the results be plotted?
		}
    \item{gumbel}{
    	[gevFit] -
	    a logical, by default FALSE. To fit a Gumbel model with fixed
	    \code{shape=0} set \code{gumbel=TRUE}.
	    }
	\item{k.blocks}{
		[gevrlevelPlot] -
	    specifies the particular return level to be estimated; default 
	    set arbitrarily to 20.
	    }
   	\item{model}{
   		[gevSim] - 
		a list with components \code{shape}, \code{location} and 
		\code{scale} giving the parameters of the GEV distribution.
		By default the shape parameter has the value 0.25, the
		location is zero and the scale is one.
		To fit random deviates from a Gumbel distribution set 
		\code{shape=0}.
		}
   	\item{n}{
   		[gevSim] -
		number of generated data points, an integer value.
		}
	\item{object}{
  		[summary][grlevelPlot] -
    	a fitted object of class \code{"gevFit"}.
    	}
    \item{type}{
    	a character string denoting the type of parameter estimation,
    	either by maximum likelihood estimation \code{"mle"}, the
    	default value, or by the probability weighted moment menthod
    	\code{"pwm"}.
    	}
   	\item{which}{
   		[plot][summary] -
	    a vector of logicals, one for each plot, denoting which plot 
	    should be displayed. Alkternatively if \code{which="ask"} the
	    user will be interactively asked which of the plots should be
	    desplayed. By default \code{which="all"}.
	    }
   	\item{x}{
   		[gevFit] -
    	data vector. In the case of \code{method="mle"} the interpretation 
    	depends on the value of block: if no block size is specified then 
    	data are interpreted as block maxima; if block size is set, then data
    	are interpreted as raw data and block maxima are calculated. 
    	[print][plot] -
    	a fitted object of class \code{"gevFit"}.
    	}
    \item{\dots}{
    	[gevFit] -
    	control parameters optionally passed to the 
    	optimization function. Parameters for the optimization
    	function are passed to components of the \code{control} argument of
    	\code{optim}. 
    	[plot][summary] -
    	arguments passed to the plot function.
    	}    

}


\value{
  
	\code{gevSim}
	\cr
	returns a vector of datapoints from the simulated series.
	\cr

  	\code{gevFit}
  	\cr
  	returns an object of class \code{gev} describing the fit.
  	\cr
  
  	\code{print.summary}
  	\cr
  	prints a report of the parameter fit.
  	\cr
  	
  	\code{summary}
  	\cr
  	performs diagnostic analysis. The method provides two different 
  	residual plots for assessing the fitted GEV model.  
  	\cr
  
  	\code{gevrlevelPlot}
  	\cr
  	returns a vector containing the lower 95\% bound of the confidence 
  	interval, the estimated return level and the upper 95\% bound. 
  	
}


\details{

	\bold{Parameter Estimation:}
	\cr\cr
	\code{gevFit} estimates the parameters either by the probability 
	weighted moment method, \code{method="pwm"} or by maximum log 
	likelihood estimation \code{method="mle"}. 
  	As a limiting case the Gumbel distribution can be selected. The 
  	summary method produces diagnostic plots for fitted GEV or Gumbel 
  	models.
  	\cr
  
  	\bold{Methods:}
  	\cr\cr
  	\code{print.gev}, \code{plot.gev} and \code{summary.gev} are
  	print, plot, and summary methods for a fitted object of class 
  	\code{gev}. Concerning the summary method, the data are 
  	converted to unit exponentially distributed residuals under null 
  	hypothesis that GEV fits. Two diagnostics for iid exponential data 
  	are offered. The plot method provides two different residual plots 
  	for assessing the fitted GEV model. Two diagnostics for 
  	iid exponential data are offered. 
  	\cr
  
  	\bold{Return Level Plot:}
  	\cr\cr
  	\code{gevrlevelPlot} calculates and plots the k-block return level 
  	and 95\% confidence interval based on a GEV model for block maxima, 
  	where \code{k} is specified by the user. The k-block return level 
  	is that level exceeded once every \code{k} blocks, on average. The 
  	GEV likelihood is reparameterized in terms of the unknown return 
  	level and profile likelihood arguments are used to construct a 
  	confidence interval. 
  	
}


\note{	
  
	If method \code{"mle"} is selected the parameter fitting in \code{gevFit} 
  	is passed to the internal function \code{gev.mle} or \code{gumbel.mle}
  	depending on the value of \code{gumbel}, \code{FALSE} or \code{TRUE}.
  	On the other hand, if method \code{"pwm"} is selected the parameter 
  	fitting in \code{gevFit} is passed to the internal function 
  	\code{gev.pwm} or \code{gumbel.pwm} again depending on the value of 
  	\code{gumbel}, \code{FALSE} or \code{TRUE}.
  		
}


\references{

Embrechts, P., Klueppelberg, C., Mikosch, T. (1997);
  	\emph{Modelling Extremal Events}, 
  	Springer.
  	
}


\author{
  
	This function is based on Alec Stephenson's R-package \code{evir} 
  	ported from the \code{EVIS} library, \emph{Extreme Values in S},
  	written by Alexander McNeil. The \code{fExtremes} port and the 
  	change and addition of some functions were done by Diethelm Wuertz.
  	
}


\seealso{
  
	\code{\link{gevglmFit}},
  	\code{\link{gevFamily}}.
  	
}

  
\examples{
## Simulate GEV Data:
   xmpExtremes("\nStart: Simulte GEV Sample >")
   # Use default length n=1000
   x = gevSim(model = list(shape = 0.25, location =0 , scale = 1))
   # Fit GEV Data by Probability Weighted Moments:
   fit = gevFit(x, type = "pwm") 
   print(fit)
   # Summarize Results:
   par(mfcol = c(3, 2))
   summary(fit)
   
## Fit GEV Data by Max Log Likelihood Method:
   xmpExtremes("\nNext: Estimate Parameters >")
   fit = gevFit(x, type = "mle") 
   print(fit)
   # Summarize Results:
   summary(fit) 
    
## Simulate Gumbel Data:
   xmpExtremes("\nNext: Simulte Gumbel Sample >")
   # Use default length n=1000
   ##> x = gevSim(model = list(shape = 0, location = 0, scale = 1))
   # Fit Gumbel Data by Probability Weighted Moments:
   ##> fit = gevFit(x, type = "pwm", gumbel = TRUE) 
   ##> print(fit)
   # Summarize Results:
   ##> par(mfcol = c(3, 2))
   ##> summary(fit)  
   
## Fit Gumbel Data by Max Log Likelihood Method:
   xmpExtremes("\nNext: Estimate Parameters >")
   ##> fit = gevFit(x, type = "mle", gumbel = TRUE) 
   ##> print(fit)
   # Summarize Results:
   ##> summary(fit)     
   ##> xmpExtremes("Press any key to continue >") 
   
## Return levels based on GEV Fit:
   # BMW Stock Data:
   xmpExtremes("\nNext: Compute BMW Return Levels >")
   par(mfrow = c(2, 1))
   data(bmw)
   # Fit GEV to monthly Block Maxima:
   fit = gevFit(-bmw, block = "month") 
   # Calculate the 40 month return level
   gevrlevelPlot(fit, k.block = 40, main = "BMW: Return Levels")   
   
## Return levels based on GEV Fit:
   xmpExtremes("\nNext: Compute SIEMENS Return Levels >")
   # Siemens Stock Data:
   data(siemens)
   # Fit GEV to monthly Block Maxima:
   fit = gevFit(-siemens, block = "month") 
   # Calculate the 40 month return level
   gevrlevelPlot(fit, k.block = 40, main = "SIEMENS: Return Levels")
   
## Interactive Plot:
   ##> par(mfrow = c(1, 1), ask = TRUE)
   ##> plot(fit)
}


\keyword{models}


\name{gevFamily}

\alias{gevFamily}

\alias{dgev}
\alias{pgev}
\alias{qgev}
\alias{rgev}

\alias{devd}
\alias{pevd}
\alias{qevd}
\alias{revd}


\title{GEV Distributions for Extreme Value Theory}


\description{

	This is a collection of distribution functions used in extreme 
	value theory. The functions compute density, distribution function, 
	quantile function and generate random deviates for the Generalized 
	Extreme Value Distribution, GEV, for the Frechet, Gumbel, and Weibull 
	distributions.
	\cr
	
	The functions are:
	
	\tabular{rll}{
	1	  \tab	\code{dgev}		
		  \tab	Density of the GEV Distribution, \cr
	2 	  \tab	\code{pgev}			
		  \tab	Probability function of the GEV Distribution, \cr
	3 	  \tab	\code{qgev}			
		  \tab	Quantile function of the GEV Distribution, \cr
	4 	  \tab	\code{rgev}		
		  \tab	Random variates from the GEV Distribution. \cr
	5-8	  \tab	\code{[dpqr]frechet}	
		  \tab	Frechet Distribution, \cr
	9-12  \tab	\code{[dpqr]gumbel}	
		  \tab	Gumbel Distribution, \cr
	13-16 \tab	\code{[dpqr]weibull}	
		  \tab	Weibull Distribution, \cr
	17-20 \tab	\code{[dpqr]evd}		
		  \tab	An alternative call for the GEV Distribution. }
	
}


\usage{
dgev(x, xi = 1, mu = 0, sigma = 1, log = FALSE)
pgev(q, xi = 1, mu = 0, sigma = 1, lower.tail = TRUE)
qgev(p, xi = 1, mu = 0, sigma = 1, lower.tail = TRUE)
rgev(n, xi = 1, mu = 0, sigma = 1)

devd(x, loc = 0, scale = 1, shape = 0, log = FALSE)
pevd(q, loc = 0, scale = 1, shape = 0, lower.tail = TRUE)
qevd(p, loc = 0, scale = 1, shape = 0, lower.tail = TRUE)
revd(n, loc = 0, scale = 1, shape = 0)
}


\arguments{

	\item{loc, scale, shape}{
		\code{loc} is the location parameter, 
		\code{scale} the scale parameter,
		and \code{shape} is the shape parameter.
		The default values are \code{loc=0}, \code{scale=1}, and 
		\code{shape=0}.
		}
	\item{log}{
	    a logical, if \code{TRUE}, the log density is returned.
	    }
  	\item{lower.tail}{
	    a logical, if \code{TRUE}, the default, then
	    probabilities are \code{P[X <= x]}, otherwise, \code{P[X > x]}.
	    }
	\item{n}{
		the number of observations.
		} 
	\item{p}{
		a numeric vector of probabilities.
		}
	\item{q}{
		a numeric vector of quantiles.
		}
	\item{x}{
		a numeric vector of quantiles.
		}
	\item{xi, mu, sigma}{
		\code{xi} is the shape parameter, 
		\code{mu} the location parameter,
		and \code{sigma} is the scale parameter.
		The default values are \code{xi=1}, \code{mu=0}, and 
		\code{sigma=1}.
		}
		
}


\value{
	
	All values are numeric vectors: \cr
	\code{d*} returns the density, \cr
	\code{p*} returns the probability, \cr
	\code{q*} returns the quantiles, and \cr
	\code{r*} generates random variates. \cr
}


\details{

	\bold{Generalized Extreme Value Distribution:}
	\cr\cr
	Computes density, distribution function, quantile function and 
	generates random variates for the Generalized Extreme Value 
	Distribution, GEV, for the Frechet, Gumbel, and Weibull 
	distributions.
		
}

\note{

	Here we implemented the notation for the arguments as used
	by the GEV functions in the EVIS package or SPlus/FinMetrics 
	module. Additional arguments to these packages are the \code{log} 
	and the \code{lower.tail} arguments, underlying the code
	from R's \code{evd} package.
	
	An alternative usage is proposed by the \code{evd} package.
	There the following arguments are used:\cr
	
	\code{*gev(x, loc = 0, scale = 1, shape = 0, ...)}\cr
	
	What you prefer is a matter of taste. The GEV functions from
	the \code{evd} package are renamed from \code{*gev} to \code{*evd}
	so that both versions are available. 
	 
	In addition functions for the density, probability, quantiles, 
	and the generation of random variates for the 
	Frechet \code{[dpqr]frechet},
	Gumbel \code{[dpqr]gumbel}, and
	Weibull \code{[dpqr]weibull} are also available.
    \cr
	
	If you stay with both versions you can access the help page for 
	\code{evds}'s function \code{dgev} by \code{help(dgev, package="evd")}.
	
}


\author{

	Alec Stephenson for the functions from R's \code{evd} package, and \cr
	Diethelm Wuertz for this \R-port.
	
}


\references{
Embrechts, P., Klueppelberg, C., Mikosch, T. (1997);
	\emph{Modelling Extremal Events}, Springer.  
}


\seealso{
  
	\code{\link{gevFit}},
  	\code{\link{gpdFit}}.
  	
}


\examples{
## *gev  -
   xmpExtremes("\nStart: GEV Frechet >")
   # Create and plot 1000 GEV/Frechet distributed rdv:
   par(mfrow = c(3, 3))
   r = rgev(n = 1000, xi = 1)
   plot(r, type = "l", main = "GEV/Frechet Series")
   ## Plot empirical density and compare with true density:
   ## Omit values greater than 500 from plot
   hist(r[r<10], n = 25, probability = TRUE, xlab = "r", 
     xlim = c(-5, 5), ylim = c(0, 1.1), main = "Density")
   x = seq(-5, 5, by=0.01)
   lines(x, dgev(x, xi = 1), col = 2)
   ## Plot df and compare with true df:
   plot(sort(r), (1:length(r)/length(r)), 
     xlim = c(-3, 6), ylim = c(0, 1.1),
   	 cex = 0.5, ylab = "p", xlab = "q", main = "Probability")
   q = seq(-5,5, by=0.1)
   lines(q, pgev(q, xi=1), col=2)
## Compute quantiles, a test:
   qgev(pgev(seq(-5, 5, 0.25), xi = 1), xi = 1) 
   
## *gev -
   xmpExtremes("\nNext: GEV Gumbel >")
   # Create and plot 1000 Gumbel distributed rdv:
   ##> r = rgev(n = 1000, xi = 0)
   ##> plot(r, type = "l", main = "Gumbel Series")
   ## Plot empirical density and compare with true density:
   ##>hist(r[abs(r)<10], nclass = 25, freq = FALSE, xlab = "r", 
   ##>	 xlim = c(-5,5), ylim = c(0,1.1), main = "Density")
   ##>x = seq(-5, 5, by = 0.01)
   ##>lines(x, dgev(x, xi = 0), col=2)
   ## Plot df and compare with true df:
   ##>plot(sort(r), (1:length(r)/length(r)), 
   ##>  xlim = c(-3, 6), ylim = c(0, 1.1),
   ##>	 cex=0.5, ylab = "p", xlab="q", main="Probability")
   ##>q = seq(-5, 5, by = 0.1)
   ##>lines(q, pgev(q, xi = 0), col = 2)
   ## Compute quantiles, a test:
   ##>qgev(pgev(seq(-5, 5, 0.25), xi = 0), xi = 0)   

## *gev -
   xmpExtremes("\nNext: GEV Weibull >")
   # Create and plot 1000 Weibull distributed rdv:
   r = rgev(n = 1000, xi = -1)
   plot(r, type = "l", main = "Weibull Series")
   ## Plot empirical density and compare with true density:
   hist(r[abs(r)<10], nclass = 25, freq = FALSE, xlab = "r", 
   	 xlim=c(-5,5), ylim=c(0,1.1), main="Density")
   x = seq(-5, 5, by=0.01)
   lines(x, dgev(x, xi = -1), col = 2)
   ## Plot df and compare with true df:
   plot(sort(r), (1:length(r)/length(r)), 
     xlim = c(-3, 6), ylim = c(0, 1.1),
   	 cex = 0.5, ylab = "p", xlab = "q", main = "Probability")
   q=seq(-5, 5, by = 0.1)
   lines(q, pgev(q, xi = -1), col = 2)
   ## Compute quantiles, a test:
   qgev(pgev(seq(-5, 5, 0.25), xi = -1), xi = -1)   
}


\keyword{distribution}


\name{MdaPlots}

\alias{MdaPlots}

\alias{hillPlot}
\alias{shaparmPlot}
\alias{shaparmPickands}
\alias{shaparmHill}
\alias{shaparmDEHaan}

\title{Hill and Shape Parameter Plots}


\description{
  
    A collection and description of functions for the 
    estimation of the tail index of extreme data using 
    the maximum domain of attraction, MDA, method. Included 
    are the Pickands, Einmal-Decker-deHaan, and Hill
    estimators together with several plot variants.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{hillPlot} \tab shape parameter and Hill estimate of the tail index, \cr
    \code{shaparmPlot} \tab variation of shape parameter with tail depth. }
    
}


\usage{
hillPlot(x, option = c("alpha", "xi", "quantile"), start = 15,
    end = NA, reverse = FALSE, p = NA, ci = 0.95, autoscale = TRUE, 
    labels = TRUE, \dots)
    
shaparmPlot(x, revert = FALSE, standardize = FALSE, tails = 0.01*(1:10), 
    doplot = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE), 
    which = c(TRUE, TRUE, TRUE), doprint = TRUE, both.tails = TRUE, 
    xi.range = c(-0.5, 1.5), alpha.range = c(0, 10)) 
}


\arguments{


    \item{x}{
        [hillPlot][shaparmPlot] - \cr
        the data from which to calculate the shape parameter, a
        numeric vector.
        }

    \item{autoscale}{
        [hillPlot] - \cr
        whether or not plot should be automatically
        scaled; if not, \code{xlim} and \code{ylim} graphical
        parameters may be entered.
        }
    \item{ci}{
        [hillPlot] - \cr
        probability for asymptotic confidence band; for no
        confidence band set \code{ci} to zero.
        }
    \item{labels}{
        [hillPlot] - \cr
        whether or not axes should be labelled.
        }
    \item{option}{
        [hillPlot] - \cr
        whether \code{alpha}, \code{xi} (1/alpha) or
        \code{quantile} (a quantile estimate) should be plotted.
        }
    \item{p}{
        [hillPlot] - \cr
        probability required when option \code{quantile} is
        chosen.
        }
    \item{reverse}{
        [hillPlot] - \cr
        whether plot is to be by increasing threshold, \code{TRUE}, or 
        increasing number of order statistics \code{FALSE}.
        }
    \item{start, end}{
        [hillPlot] - \cr
        lowest and highest number of order statistics at which to plot
        a point.
        }
    \item{\dots}{
        [hillPlot] - \cr
        other graphics parameters.
        }


    \item{alpha.range, xi.range}{
    	[saparmPlot] - \cr
        plotting ranges.
        }
     \item{both.tails}{
     	[shaparmPlot] - \cr
        a logical, decides whether or not both tails should be 
        investigated. By default TRUE. If FALSE only the lower
        tail will be investigated.
        }
    \item{doplot}{
    	[shaparmPlot] - \cr
        a vector of logicals of the same lengths as tails
        defining for wich tail depths plots should be created,
        by default plots will be generated for a tail depth of 5
        percent. By default \code{c(FALSE, FALSE, FALSE, FALSE, 
        TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)}.
        }
    \item{doprint}{
    	[shaparmPlot] - \cr
        a logical, decides whether or not for all tail depths the
        result for the shape parameter \code{1/alpha} should be
        printed.
        }
    \item{revert}{
    	[shaparmPlot] - \cr
        a logical value, by default FALSE, if set to TRUE the
        sign of the vector will be reverted: \code{x = -x}.
        }
    \item{standardize}{
    	[shaparmPlot] - \cr
        a logical value, by default FALSE, if set to 
        TRUE the vector \code{x} will be standardized: 
        \code{x = (x-mean(x))/sqrt(var(x))}.
        }
    \item{tails}{
    	[shaparmPlot] - \cr
        a numeric vector of tail depths to be considered; by
        default ten values ranging from 0.1 to 1.0 in steps of 0.1
        corresponding to values ranging from 1 to 10 percent.
        }
    \item{which}{
    	[shaparmPlot] - \cr
        a vector of 3 logicals indicating which plots from the 
        three methods will be created. The first entry decides
        for the Pickands estimator, the second for the Hill 
        estimator, and the last for the Deckers-Einmahl-deHaan
        estimator. By default all three will be created.
        By default \code{c(TRUE, TRUE, TRUE)}.
        }

        
}

\value{ 
  
    \code{hillPlot}
    \cr
    displays a plot.
   
    \code{shaparmPlot} 
    \cr
    returns a list with one or two entries, depending on the
    selection of the input variable \code{both.tails}. The two 
    entries \code{upper} and \code{lower} determine the position of 
    the tail. Each of the two variables is again a list with entries 
    \code{pickands}, \code{hill}, and \code{dehaan}. If one of the 
    three methods will be discarded the printout will display zeroes.
    
}


\details{

    \bold{Hill Plot:}
    \cr\cr
    The function \code{hillPlot} investigates the shape parameter and 
    plots the Hill estimate of the tail index of heavy-tailed data, or 
    of an associated quantile estimate. This plot is usually calculated 
    from the alpha perspective. For a generalized Pareto analysis of 
    heavy-tailed data using the \code{gpdFit} function, it helps to 
    plot the Hill estimates for \code{xi}. 
    \cr
  
    \bold{Shape Parameter Plot:}
    \cr\cr
    The function \code{shaparmPlot} investigates the shape parameter and 
    plots for the upper and lower tails the shape parameter as a function 
    of the taildepth. Three approaches are considered, the \emph{Pickands} 
    estimator, the \emph{Hill} estimator, and the
    \emph{Decker-Einmal-deHaan} estimator.
    
    
}


\author{ 
  
    Alec Stephenson for \R's evir package, \cr
    Alexander Mcneil for the original EVIS code, \cr
    Diethelm Wuertz for this \R-port. 
    
}


\references{

Coles S. (2001);
    \emph{Introduction to Statistical Modelling of Extreme Values},
    Springer.
    
Embrechts, P., Klueppelberg, C., Mikosch, T. (1997);
    \emph{Modelling Extremal Events}, Springer.  
    
}


\seealso{

    \code{\link{gpdFit}}.
    
}


\examples{
## hillPlot -
   xmpExtremes("\nStart: Hill Estimator >")
   # Hill plot of heavy-tailed Danish fire insurance data 
   # and BMW stock data for estimated 0.999 quantile
   par(mfrow = c(2, 2))
   data(bmw)
   hillPlot(bmw)
   hillPlot(bmw, option = "quantile", end = 500, p = 0.999)
   data(danish)
   hillPlot(danish)
   hillPlot(danish, option = "quantile", end = 500, p = 0.999)
   
## shaparmPlot -
   xmpExtremes("\nNext: Shape Parameter Plots >")
   par(mfcol = c(3, 2), cex = 0.6)
   data(bmw)
   shaparmPlot(bmw)

## shaparmPlot -
   xmpExtremes("\nNext: Simulated Frechet Data >")
   par(mfcol = c(3, 2), cex = 0.6)
   set.seed(4711)
   x = rgev(10000, xi = 1/4)
   shaparmPlot(x, revert = TRUE, both.tails = FALSE)
   lines(c(0.01, 0.1), c(4, 4), col = "steelblue3") # True Value
}


\keyword{math}


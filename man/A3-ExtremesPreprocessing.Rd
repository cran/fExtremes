\name{ExtremesPreprocessing}

\alias{ExtremesPreprocessing}

\alias{findThreshold}
\alias{blocks}
\alias{blockMaxima}
\alias{deCluster}

\title{Preprocessing Extreme Value Data}

\description{

    A collection and description of functions for preprocessing 
    data for extreme value analysis. Included are tools to 
    separate data beyond a threshold value, to compute blockwise 
    data like block maxima, and to decluster point process data.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{findThreshold} \tab Upper threshold for a given number of extremes, \cr  
    \code{blocks} \tab  Create data blocks on vectors and time series, \cr
    \code{blockMaxima} \tab Block Maxima from a vector or a time series, \cr 
    \code{deCluster} \tab Declusters clustered point process data. }
    
}


\usage{
findThreshold(x, n = NA)
blocks(x, block = "month", FUN = max)
blockMaxima(x, block = "month", details = FALSE, doplot = TRUE, \dots)
deCluster(x, run = NA, doplot = TRUE)
}


\arguments{
  
    \item{block}{
        the block size. A numeric value is interpreted as the number  
        of data values in each successive block. All the data is used,
        so the last block may not contain \code{block} observations. 
        If the \code{data} has a  \code{times} attribute containing (in
        an object of class \code{"POSIXct"}, or an object that can be
        converted to that class, see \code{\link{as.POSIXct}}) the
        times/dates of each observation, then \code{block} may instead
        take the character values \code{"month"}, \code{"quarter"},
        \code{"semester"} or \code{"year"}. By default monthly blocks
        from daily data are assumed.
        }
    \item{details}{
        [blockMaxima] - \cr
        a logical. Should details be printed?
        }
    \item{doplot}{
        a logical. Should the results be plotted?
        }
    \item{FUN}{the function to be applied. Additional arguments are
        passed by the \code{\dots} argument.
        }
    \item{n}{
        [findThreshold] - \cr
        a numeric value or vector giving number of extremes above 
        the threshold. If \code{n} is not specified, \code{n} is 
        set to an integer representing 5\% of the data from the 
        whole data set \code{x}.
        }
    \item{run}{
        [deCluster] - \cr
        parameter to be used in the runs method; any two consecutive 
        threshold exceedances separated by more than this number of 
        observations/days are considered to belong to different clusters.
        }    
    \item{x}{
        a numeric data vector from which \code{findThreshold} and 
        \code{blockMaxima} determine the threshold values and block 
        maxima values. 
        For the function \code{deCluster} the argument
        \code{x} represents a numeric vector of threshold exceedances
        with a \code{times} attribute which should be a numeric
        vector containing either the indices or the times/dates
        of each exceedance (if times/dates, the attribute should
        be an object of class \code{"POSIXct"} or an object that
        can be converted to that class; see \code{\link{as.POSIXct}}).
        }
    \item{\dots}{
        additional arguments passed to the FUN or plot function.
        }
        
}


\details{
  
    \bold{Finding Thresholds:} 
    \cr\cr
    The function \code{findThreshold} finds a threshold so that a given 
    number of extremes lie above. When the data are tied a threshold is 
    found so that at least the specified number of extremes lie above.
    \cr
    
  
    \bold{Computing Block Maxima:} 
    \cr\cr  
    The function \code{blockMaxima} calculates block maxima from a vector 
    or a time series, whereas the function
    \code{blocks} is more general and allows for the calculation of
    an arbitrary function \code{FUN} on blocks.
    \cr
    
        
    \bold{De-Clustering Point Processes:} 
    \cr\cr
    The function \code{deCluster} declusters clustered point process 
    data so that Poisson assumption is more tenable over a high threshold.
        
}


\value{
  
    \code{findThreshold} 
    \cr
    returns a numeric vector of suitable thresholds. 
    
    \code{blockMaxima} 
    \cr
    returns a numeric vector of block maxima data.
    
    \code{deCluster} 
    \cr
    returns an object for the declustered point process. 

}


\references{

Embrechts, P., Klueppelberg, C., Mikosch, T. (1997);
    \emph{Modelling Extremal Events},
    Springer Verlag. 
     
}



\seealso{
  
    \code{\link{MdaPlots}}, 
    \code{\link{ExtremeIndexPlots}}, 
    \code{\link{GpdFit}},
    \code{\link{PotFit}}.
    
}


\examples{  
## findThreshold -
   xmpExtremes("\nStart: Find Thresold >")
   # Find threshold giving (at least) fifty exceedances 
   # for Danish Fire data
   data(danish)
   findThreshold(danish, n = c(10, 50, 100))    
   
## blockMaxima -
   xmpExtremes("\nNext: Compute Block Maxima >")
   # Block Maxima (Minima) for the right and left tails 
   # of the BMW log returns:
   data(bmw)
   par(mfrow = c(2, 1))
   blockMaxima( bmw, block = 100)
   blockMaxima(-bmw, block = 100)     
 
## deCluster -
   xmpExtremes("\nNext: De-Cluster Exceedences >")
   # Decluster the 200 exceedances of a particular  
   # threshold in the negative BMW log-return data
   par(mfrow = c(2, 2))
   fit = potFit(-bmw, nextremes = 200) 
   deCluster(fit$fit$data, 30)   
}


\keyword{data}


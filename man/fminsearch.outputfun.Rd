\name{fminsearch.outputfun}
\alias{fminsearch.outputfun}
\title{
  fminsearch Output Function Call
}
\description{
  This function calls the output function and make it match neldermead
  requirements. It is used in the \code{fminsearch} function as the
  \code{outputcommand} element of the neldermead object (see
  \code{?neldermead.new} and \code{?neldermead.configure}).
}
\usage{
  fminsearch.outputfun(state = NULL, data = NULL, fmsdata = NULL)
}
\arguments{
  \item{state}{The current state of the algorithm either 'init', 'iter' or
    'done'.}
  \item{data}{The data at the current state. This is a list with a 'type'
    attribute set to 'T_NMDATA' and  with the following elements:\describe{
      \item{x}{The current parameter estimates.}
      \item{fval}{The current value of the cost function.}
      \item{simplex}{The current simplex object.}
      \item{iteration}{The number of iterations performed.}
      \item{funccount}{The number of function evaluations.}
      \item{step}{The type of step in the previous iteration.}
    }
  }
  \item{fmsdata}{This is a list with a 'type' attribute set to 'T_FARGS'
    which contains specific data of the \code{fminsearch} algorithm: \describe{
      \item{Display}{what to display}
      \item{OutputFcn}{the array of output functions}
      \item{PlotFcns}{the array of plot functions}
    }
  }
}
\value{
  This function does not return any data, but execute the output function(s).
}
\author{
Author of Scilab neldermead module: Michael Baudin (INRIA - Digiteo)

Author of R adaptation: Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\seealso{
  \code{\link[neldermead]{fminsearch}},
  \code{\link[neldermead]{neldermead.new}},
  \code{\link[neldermead]{neldermead.configure}},
}
\keyword{ method }

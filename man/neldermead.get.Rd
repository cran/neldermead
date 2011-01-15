\name{Get functions}
\alias{neldermead.get}
\alias{neldermead.cget}
\title{
  Get the value for the given key
}
\description{
  Get the value for the given key in a neldermead object.
}
\usage{
  neldermead.get(this = NULL, key = NULL)
  neldermead.cget(this = NULL, key = NULL)
}
\arguments{
  \item{this}{A neldermead object.}
  \item{key}{The name of the key to quiery. The list of available keys for
    query with \code{neldermead.get} is: '-historysimplex',
    '-simplexopt', '-simplex0', and '-restartnb'. If \code{key} is different,
    the query is delegated to \code{optimbase.get}.

    The list of available keys for query with \code{neldermead.cget} is:
    '-method', '-coords0', '-simplex0method', '-simplex0length',
    '-simplex0deltausual', '-simplex0deltazero', '-rho', '-chi', '-gamma',
    '-sigma', '-tolsimplexizeabsolute', '-tolsimplexizerelative',
    '-tolsimplexizemethod', '-toldeltafv', '-tolssizedeltafvmethod',
    '-restartmax', '-restarteps', '-restartstep', '-kelleystagnationflag',
    '-kelleynormalizationflag', '-kelleystagnationalpha0', '-restartflag',
    '-restartdetection', '-restartsimplexmethod', '-checkcostfunction',
    '-boxnbpoints', '-boxineqscaling', '-scalingsimplex0', '-guinalphamin',
    '-boxtermination', '-boxtolf', '-boxnbmatch', '-boxreflect', '-mymethod',
    '-myterminate', '-myterminateflag', '-tolvarianceflag',
    '-tolabsolutevariance', '-tolrelativevariance', and '-greedy'. If \code{key}
    is different, the query is delegated to \code{optimbase.cget}.
  }
}
\value{
  Return the value of the list element \code{key}, or an error message if
  \code{key} does not exist.
}
\author{
Author of Scilab neldermead module: Michael Baudin (INRIA - Digiteo)

Author of R adaptation: Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\seealso{
  \code{\link[neldermead]{neldermead.configure}},
  \code{\link[optimbase]{optimbase.cget}},
  \code{\link[optimbase]{optimbase.get}}
}
\keyword{ method }

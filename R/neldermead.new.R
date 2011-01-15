# Copyright (C) 2008-2009 - INRIA - Michael Baudin
# Copyright (C) 2009-2010 - DIGITEO - Michael Baudin
# Copyright (C) 2010-2011 - Sebastien Bihorel
#
# This file must be used under the terms of the CeCILL.
# This source file is licensed as described in the file COPYING, which
# you should have received as part of this distribution. The terms
# are also available at
# http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
#
# This source code is a R port of the neldermead component
# originally written by Michael Baudin for Scilab :
# "Nelder-Mead User's Manual", 2010, Consortium Scilab - Digiteo,
# Michael Baudin, http://wiki.scilab.org/The_Nelder-Mead_Component

neldermead.new <- function(){
  newobj <- list(optbase=c(),
                 method=c(),
                 simplex0=c(),
                 simplex0method=c(),
                 simplex0length=c(),
                 rho=c(),
                 chi=c(),
                 gamma=c(),
                 sigma=c(),
                 tolfstdeviation=c(),
                 tolfstdeviationmethod=c(),
                 tolsimplexizeabsolute=c(),
                 tolsimplexizerelative=c(),
                 tolsimplexizemethod=c(),
                 simplexsize0=c(),
                 toldeltafv=c(),
                 tolssizedeltafvmethod=c(),
                 historysimplex=c(),
                 coords0=c(),
                 simplex0deltausual=c(),
                 simplex0deltazero=c(),
                 restartsimplexmethod=c(),
                 simplexopt=c(),
                 restartmax=c(),
                 restarteps=c(),
                 restartstep=c(),
                 kelleystagnationflag=c(),
                 kelleynormalizationflag=c(),
                 kelleystagnationalpha0=c(),
                 kelleyalpha=c(),
                 restartnb=c(),
                 restartflag=c(),
                 restartdetection=c(),
                 startupflag=c(),
                 boxnbpoints=c(),
                 boxnbpointseff=c(),
                 boxineqscaling=c(),
                 checkcostfunction=c(),
                 scalingsimplex0=c(),
                 guinalphamin=c(),
                 boxboundsalpha=c(),
                 boxtermination=c(),
                 boxtolf=c(),
                 boxnbmatch=c(),
                 boxkount=c(),
                 boxreflect=c(),
                 tolvarianceflag=c(),
                 tolabsolutevariance=c(),
                 tolrelativevariance=c(),
                 variancesimplex0=c(),
                 mymethod=c(),
                 myterminate=c(),
                 myterminateflag=c(),
                 greedy=c())

  attr(newobj,'type') <- 'T_NELDERMEAD'

  newobj$optbase <- optimbase.new()

  # Possible values "variable", "fixed".
  newobj$method <- 'variable'
  newobj$simplex0 <- optimsimplex.new()$newobj

  # Possible values : "axes", "spendley", "pfeffer"
  newobj$simplex0method <- 'axes'
  newobj$simplex0length <- 1.0

  # Reflection factor : rho
  newobj$rho <- 1.0

  # Expansion factor : chi
  newobj$chi <- 2.0

  # Contraction factor : gamma
  newobj$gamma <- .5

  # Shrinkage factor : sigma
  newobj$sigma <- .5

  # The tolerance for the standard deviation
  newobj$tolfstdeviation <- 0.0
  
  # Possible values : TRUE, FALSE
  newobj$tolfstdeviationmethod <- FALSE
  
  # The absolute tolerance for the simplex size
  newobj$tolsimplexizeabsolute <- 0.0

  # The relative tolerance for the simplex size
  newobj$tolsimplexizerelative <- .Machine$double.eps
  
  # Possible values : TRUE, FALSE
  # Note : If the simplex method converges, the simplex size is near zero.
  newobj$tolsimplexizemethod <- TRUE

  # The tolerance for the function value delta
  newobj$toldeltafv <- .Machine$double.eps

  # Possible values : TRUE, FALSE
  newobj$tolssizedeltafvmethod <- FALSE

  # The value used in Pfeffer method initial simplex computation for non-zero parameters
  newobj$simplex0deltausual <- 0.05
  
  # The value used in Pfeffer method initial simplex computation for zero parameters
  newobj$simplex0deltazero <- 0.0075
  
  # The coordinates of the initial simplex, given by the user
  newobj$coords0 <- c()
  
  # The Kelley stagnation detection in termination criteria :  0/1
  # (i.e. sufficient decrease of function value)
  newobj$kelleystagnationflag <- FALSE

  # The Kelley stagnation detection parameter
  newobj$kelleystagnationalpha0 <- 1.e-4
  
  # The Kelley stagnation detection can be normalized or not.
  # Note:
  # * in the 1997 paper "Detection and Remediation of Stagnation in Nelder-Mead
  #   algorithm", Kelley uses the constant value of 1.e-4.
  # * in the 1999 book "Iterative Methods for Optimization", Kelley uses normalization.
  # Results are slightly changed, as indicated in the book/paper (the modification is
  # not mentioned, but the iteration number when the restart is performed
  # is modified).
  newobj$kelleynormalizationflag <- TRUE

  # The current value of Kelley's alpha, after normalization, if required
  newobj$kelleyalpha <- 1.e-4
  
  # The optimum simplex, after one optimization process
  # newobj$simplexopt <- NULL
  
  # The maximum number of restarts
  newobj$restartmax <- 3
  
  # The epsilon value for O'Neill restart detection
  newobj$restarteps <- .Machine$double.eps
  
  # The step length for O'Neill restart detection
  newobj$restartstep <- 1.0
  
  # Possible values : "oriented", "axes", "spendley", "pfeffer"
  newobj$restartsimplexmethod <- 'oriented'
  
  # Possible values : TRUE, FALSE
  newobj$restartflag <- FALSE
  
  # Number of restarts performed
  newobj$restartnb <- 0
  
  # Type of restart detection method : "kelley", "oneill"
  newobj$restartdetection <- 'oneill'
  
  # Set to TRUE when the startup has been performed
  newobj$startupflag <- FALSE
  
  # Initial size of the simplex, for the tolerance on the simplex size
  newobj$simplexsize0 <- 0.0
  
  # Number of points required in the simplex (for Box method)
  newobj$boxnbpoints <- '2n'
  
  # Effective number of points required in the simplex (for Box method)
  newobj$boxnbpointseff <- 0
  
  # The scaling coefficient in nonlinear inequality constraints
  # in Box method, in (0,1) range
  newobj$boxineqscaling <- 0.5
  
  # Set to FALSE to disable the checking of the connection of the cost function
  newobj$checkcostfunction <- TRUE
  
  # The scaling algorithm : "tox0", "tocentroid"
  newobj$scalingsimplex0 <- 'tox0'
  
  # Minimum alpha for constraints scaling
  newobj$guinalphamin <- 1.e-5
  
  # Box's alpha coefficient for bounds constraints.
  # The value used in Box's paper was 1.e-6 (delta in
  # Richardson and Kuester's algorithm 454)
  newobj$boxboundsalpha <- 1.e-6
  
  # Set to 1 to enable Box termination criteria
  newobj$boxtermination <- FALSE
  
  # The absolute tolerance on function value in Box termination criteria (beta in
  # Richardson and Kuester's algorithm 454)
  newobj$boxtolf <- 1.e-5
  
  # The number of consecutive match in Box termination criteria (gamma in
  # Richardson and Kuester's algorithm 454)
  newobj$boxnbmatch <- 5
  
  # Current number of consecutive match
  newobj$boxkount <- 0
  
  # Box reflection/expansion factor
  newobj$boxreflect <- 1.3
  
  # Set to TRUE to enable tolerance on variance
  newobj$tolvarianceflag <- FALSE
  
  # Absolute tolerance on variance
  newobj$tolabsolutevariance <- 0.0
  
  # Relative tolerance on variance
  newobj$tolrelativevariance <- .Machine$double.eps
  
  # The variance of the initial simplex
  newobj$variancesimplex0 <- 0.0
  
  # User-defined algorithm
  # newobj$mymethod <- NULL
  
  # User-defined terimination criteria
  # newobj$myterminate <- NULL
  
  # Flag to enable the user-defined terimination criteria
  newobj$myterminateflag <- FALSE
  
  # Set to TRUE to enable greedy Nelder-Mead
  newobj$greedy <- FALSE

  return(newobj)
  
}


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

optimset <- function(method=NULL,Display=NULL,FunValCheck=NULL,MaxFunEvals=NULL,
                     MaxIter=NULL,OutputFcn=NULL,PlotFcns=NULL,TolFun=NULL,
                     TolX=NULL){

  # Check inputs
  varargin <- as.list(match.call())[-1]
  nargin <- length(varargin)

  options <- list(Display=NULL,
                  FunValCheck=NULL,
                  MaxFunEvals=NULL,
                  MaxIter=NULL,
                  OutputFcn=NULL,
                  PlotFcns=NULL,
                  TolFun=NULL,
                  TolX=NULL)
                    
  if (nargin==0){
    return(options)
  }

  if (!is.null(method)){
    options <- optimset.method(method=method)
    return(options)
  }

  #
  # Process key values
  #
  keys <- c('Display','FunValCheck','MaxFunEvals','MaxIter','OutputFcn','PlotFcns',
            'TolFun','TolX')

  for (key in keys){
    # Use suppressWarnings to enable the set of a function into the variable
    # "value". If not, a warning message is triggered, when a double value
    # is stored into "value" after a function has already been
    # stored in it.
    if (!is.null(eval(parse(text=key))))
      suppressWarnings(options[[key]] <- eval(parse(text=key)))
  }
  
  return(options)
  
}


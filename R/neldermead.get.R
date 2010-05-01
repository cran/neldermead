# Copyright (C) 2008-2009 - INRIA - Michael Baudin
# Copyright (C) 2009-2010 - DIGITEO - Michael Baudin
# Copyright (C) 2010 - Sebastien Bihorel
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

neldermead.get <- function(this=NULL,key=NULL){

  if (!any(key==c('-historysimplex','-simplexopt','-simplex0','-restartnb'))){
    # Delegate to optbase
    value <- optimbase.get(this=this$optbase,key=key)
  }
  
  if (key=='-historysimplex'){
    storehistory <- optimbase.cget(this=this$optbase,key='-storehistory')
    if (!storehistory){
      stop('neldermead.get: History disabled ; turn on -storehistory option.',
           call.=FALSE)
    } else {
      value <- this$historysimplex
    }
  }
  if (key=='-simplexopt'){
    value <- this$simplexopt
  }
  if (key=='-simplex0'){
    value <- this$simplex0
  }
  if (key=='-restartnb'){
    value <- this$restartnb
  }

  return(value)

}


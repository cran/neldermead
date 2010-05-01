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

neldermead.storehistory <- function(this=NULL,n=NULL,fopt=NULL,xopt=NULL,
                                    xcoords=NULL){

  storehistory <- optimbase.cget(this=this$optbase,key='-storehistory')
  iterations <- optimbase.get(this=this$optbase,key='-iterations')
  if (storehistory){
    this$optbase <- optimbase.histset(this=this$optbase,iter=iterations,key='-fopt',value=fopt)
    this$optbase <- optimbase.histset(this=this$optbase,iter=iterations,key='-xopt',value=xopt[1:n])
    this$historysimplex[[iterations]] <- xcoords[1:n+1,1:n]
  }
  return(this)
}


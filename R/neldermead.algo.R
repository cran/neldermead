# Copyright (C) 2008-2009 - INRIA - Michael Baudin
# Copyright (C) 2009-2010 - DIGITEO - Michael Baudin
# Copyright (C) 2010-2022  - Sebastien Bihorel
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

neldermead.algo <- function(this=NULL){

  if (!any(this$method==c('fixed','variable','box','mine')))
    stop(sprintf('neldermead.algo: Unknown method %s',this$method),
         call.=FALSE)

  if (this$method=='fixed') this <- neldermead.fixed(this=this)
  if (this$method=='variable') this <- neldermead.variable(this=this)
  if (this$method=='box') this <- neldermead.box(this)
  if (this$method=='mine') this <- this$mymethod(this)

  return(this)

}


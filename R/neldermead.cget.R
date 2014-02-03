# Copyright (C) 2008-2009 - INRIA - Michael Baudin
# Copyright (C) 2009-2010 - DIGITEO - Michael Baudin
# Copyright (C) 2010-2014 - Sebastien Bihorel
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

neldermead.cget <- function(this=NULL,key=NULL){

  if (!any(key==c('-method','-coords0','-simplex0method','-simplex0length',
                  '-simplex0deltausual','-simplex0deltazero','-rho','-chi',
                  '-gamma','-sigma','-tolsimplexizeabsolute','-tolsimplexizerelative',
                  '-tolsimplexizemethod','-toldeltafv','-tolssizedeltafvmethod',
                  '-restartmax','-restarteps','-restartstep','-kelleystagnationflag',
                  '-kelleynormalizationflag','-kelleystagnationalpha0',
                  '-restartflag','-restartdetection','-restartsimplexmethod',
                  '-checkcostfunction','-boxnbpoints','-boxineqscaling',
                  '-scalingsimplex0','-guinalphamin',
                  '-boxtermination','-boxtolf','-boxnbmatch','-boxreflect',
                  '-mymethod','-myterminate','-myterminateflag','-tolvarianceflag',
                  '-tolabsolutevariance','-tolrelativevariance','-greedy'))){
    # Delegate to the optimization object
    value <- optimbase.cget(this=this$optbase,key=key)
  }

  if (key=='-method'){
    value <- this$method
  }
  if (key=='-coords0'){
    value <- this$coords0
  }
  if (key=='-simplex0method'){
    value <- this$simplex0method
  }
  if (key=='-simplex0length'){
    value <- this$simplex0length
  }
  if (key=='-simplex0deltausual'){
    value <- this$simplex0deltausual
  }
  if (key=='-simplex0deltazero'){
    value <- this$simplex0deltazero
  }
  if (key=='-rho'){
    value= this$rho
  }
  if (key=='-chi'){
    value <- this$chi
  }
  if (key=='-gamma'){
    value <- this$gamma
  }
  if (key=='-sigma'){
    value <- this$sigma
  }
  if (key=='-tolsimplexizemethod'){
    value <- this$tolsimplexizemethod
  }
  if (key=='-tolsimplexizeabsolute'){
    value <- this$tolsimplexizeabsolute
  }
  if (key=='-tolsimplexizerelative'){
    value <- this$tolsimplexizerelative
  }
  if (key=='-toldeltafv'){
    value <- this$toldeltafv
  }
  if (key=='-tolssizedeltafvmethod'){
    value <- this$tolssizedeltafvmethod
  }
  if (key=='-restartmax'){
    value <- this$restartmax
  }
  if (key=='-restarteps'){
    value <- this$restarteps
  }
  if (key=='-restartstep'){
    value <- this$restartstep
  }
  if (key=='-kelleystagnationflag'){
    value <- this$kelleystagnationflag
  }
  if (key=='-kelleynormalizationflag'){
    value <- this$kelleynormalizationflag
  }
  if (key=='-kelleystagnationalpha0'){
    value <- this$kelleystagnationalpha0
  }
  if (key=='-restartflag'){
    value <- this$restartflag
  }
  if (key=='-restartdetection'){
    value <- this$restartdetection
  }
  if (key=='-restartsimplexmethod'){
    value <- this$restartsimplexmethod
  }
  if (key=='-boxnbpoints'){
    value <- this$boxnbpoints
  }
  if (key=='-checkcostfunction'){
    value <- this$checkcostfunction
  }
  if (key=='-scalingsimplex0'){
    value <- this$scalingsimplex0
  }
  if (key=='-guinalphamin'){
    value <- this$guinalphamin
  }
  if (key=='-boxtermination'){
    value <- this$boxtermination
  }
  if (key=='-boxtolf'){
    value <- this$boxtolf
  }
  if (key=='-boxnbmatch'){
    value <- this$boxnbmatch
  }
  if (key=='-boxreflect'){
    value <- this$boxreflect
  }
  if (key=='-boxineqscaling'){
    value <- this$boxineqscaling
  }
  if (key=='-mymethod'){
    value <- this$mymethod
  }
  if (key=='-myterminate'){
    value <- this$myterminate
  }
  if (key=='-myterminateflag'){
    value <- this$myterminateflag
  }
  if (key=='-tolvarianceflag'){
    value <- this$tolvarianceflag
  }
  if (key=='-tolabsolutevariance'){
    value <- this$tolabsolutevariance
  }
  if (key=='-tolrelativevariance'){
    value <- this$tolrelativevariance
  }
  if (key=='-greedy'){
    value <- this$greedy
  }

  return(value)

}


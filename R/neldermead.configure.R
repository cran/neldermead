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

neldermead.configure <- function(this=NULL,
                                 key=NULL,
                                 value=NULL){

  if (!any(key==c('-method','-coords0','-simplex0method','-simplex0length',
                  '-simplex0deltausual','-simplex0deltazero','-rho','-chi',
                  '-gamma','-sigma','-tolsimplexizeabsolute','-tolsimplexizerelative',
                  '-tolsimplexizemethod','-toldeltafv','-tolssizedeltafvmethod',
                  '-restartmax','-restarteps','-restartstep','-kelleystagnationflag',
                  '-kelleynormalizationflag','-kelleystagnationalpha0',
                  '-restartflag','-restartdetection','-restartsimplexmethod',
                  '-checkcostfunction','-boxnbpoints','-boxineqscaling',
                  '-scalingsimplex0','-guinalphamin','-boxboundsalpha',
                  '-boxtermination','-boxtolf','-boxnbmatch','-boxreflect',
                  '-mymethod','-myterminate','-myterminateflag','-tolvarianceflag',
                  '-tolabsolutevariance','-tolrelativevariance','-greedy'))){
    # Delegate to the optimization object
    this$optbase <- optimbase.configure(this=this$optbase,key=key,value=value)
  }

  if (key=='-method'){
    assert.typestring(var=value,varname='value',ivar=3)
    if (!any(value==c('fixed','variable','box','mine'))){
      unknownValueForOption(value=value,optionname='-method')
    } else {
      this$method <- value
    }
  }
  if (key=='-coords0'){
    assert.typestring(var=value,varname='value',ivar=3)
    this$coords0 <- value
  }
  if (key=='-simplex0method'){
    assert.typestring(var=value,varname='value',ivar=3)
    if (!any(value==c('given','axes','spendley','pfeffer','randbounds'))){
      unknownValueForOption(value=value,optionname='-simplex0method')
    } else {
      this$simplex0method <- value
    }
  }
  if (key=='-simplex0length'){
    assert.typereal(var=value,varname='value',ivar=3)
    this$simplex0length <- value
  }
  if (key=='-simplex0deltausual'){
    assert.typereal(var=value,varname='value',ivar=3)
    this$simplex0deltausual <- value
  }
  if (key=='-simplex0deltazero'){
    assert.typereal(var=value,varname='value',ivar=3)
    this$simplex0deltazero <- value
  }
  if (key=='-rho'){
    assert.typereal(var=value,varname='value',ivar=3)
    this$rho <- value
  }
  if (key=='-chi'){
    assert.typereal(var=value,varname='value',ivar=3)
    this$chi <- value
  }
  if (key=='-gamma'){
    assert.typereal(var=value,varname='value',ivar=3)
    this$gamma <- value
  }
  if (key=='-sigma'){
    assert.typereal(var=value,varname='value',ivar=3)
    this$sigma <- value
  }
  if (key=='-tolsimplexizeabsolute'){
    assert.typereal(var=value,varname='value',ivar=3)
    this$tolsimplexizeabsolute <- value
  }
  if (key=='-tolsimplexizerelative'){
    assert.typereal(var=value,varname='value',ivar=3)
    this$tolsimplexizerelative <- value
  }
  if (key=='-tolsimplexizemethod'){
    assert.typeboolean(var=value,varname='value',ivar=3)
    if (!is.logical(value)){
      unknownValueForOption(value=value,optionname='-tolsimplexizemethod')
    } else {
      this$tolsimplexizemethod <- value
    }
  }
  if (key=='-toldeltafv'){
    assert.typereal(var=value,varname='value',ivar=3)
    this$toldeltafv <- value
  }
  if (key=='-tolssizedeltafvmethod'){
    assert.typeboolean(var=value,varname='value',ivar=3)
    if (!is.logical(value)){
      unknownValueForOption(value=value,optionname='-tolssizedeltafvmethod')
    } else {
      this$tolssizedeltafvmethod <- value
    }
  }
  if (key=='-restartmax'){
    assert.typereal(var=value,varname='value',ivar=3)
    this$restartmax <- value
  }
  if (key=='-restarteps'){
    assert.typereal(var=value,varname='value',ivar=3)
    this$restarteps <- value
  }
  if (key=='-restartstep'){
    assert.typereal(var=value,varname='value',ivar=3)
    this$restartstep <- value
  }
  if (key=='-kelleystagnationflag'){
    assert.typeboolean(var=value,varname='value',ivar=3)
    this$kelleystagnationflag <- value
  }
  if (key=='-kelleynormalizationflag'){
    assert.typeboolean(var=value,varname='value',ivar=3)
    this$kelleynormalizationflag <- value
  }
  if (key=='-kelleystagnationalpha0'){
    assert.typereal(var=value,varname='value',ivar=3)
    this$kelleystagnationalpha0 <- value
  }
  if (key=='-restartflag'){
    assert.typeboolean(var=value,varname='value',ivar=3)
    this$restartflag <- value
  }
  if (key=='-restartdetection'){
    assert.typestring(var=value,varname='value',ivar=3)
    this$restartdetection <- value
  }
  if (key=='-restartsimplexmethod'){
    assert.typestring(var=value,varname='value',ivar=3)
    this$restartsimplexmethod <- value
  }
  if (key=='-checkcostfunction'){
    assert.typeboolean(var=value,varname='value',ivar=3)
    this$restartflag <- value
    if (!is.logical(value)){
      unknownValueForOption(value=value,optionname='-checkcostfunction')
    } else {
      this$checkcostfunction <- value
    }
  }
  if (key=='-boxnbpoints'){
    assert.typereal(var=value,varname='value',ivar=3)
    this$boxnbpoints <- value
  }
  if (key=='-boxineqscaling'){
    assert.typereal(var=value,varname='value',ivar=3)
    this$boxineqscaling <- value
  }
  if (key=='-scalingsimplex0'){
    assert.typestring(var=value,varname='value',ivar=3)
    this$scalingsimplex0 <- value
  }
  if (key=='-guinalphamin'){
    assert.typereal(var=value,varname='value',ivar=3)
    if (value<=0)
      stop(sprintf('neldermead.configure: Unexpected negative value %s for -guinalphamin.',value),
           call.=FALSE)
    this$guinalphamin <- value
  }
  if (key=='-boxboundsalpha'){
    assert.typereal(var=value,varname='value',ivar=3)
    this$boxboundsalpha <- value
  }
  if (key=='-boxtermination'){
    assert.typeboolean(var=value,varname='value',ivar=3)
    this$boxtermination <- value
  }
  if (key=='-boxtolf'){
    assert.typereal(var=value,varname='value',ivar=3)
    this$boxtolf <- value
  }
  if (key=='-boxnbmatch'){
    assert.typereal(var=value,varname='value',ivar=3)
    this$boxnbmatch <- value
  }
  if (key=='-boxreflect'){
    assert.typereal(var=value,varname='value',ivar=3)
    this$boxreflect <- value
  }
  if (key=='-mymethod'){
    assert.typefunction(var=value,varname='value',ivar=3)
    this$mymethod <- value
  }
  if (key=='-myterminate'){
    assert.typefunction(var=value,varname='value',ivar=3)
    this$myterminate <- value
  }
  if (key=='-myterminateflag'){
    assert.typeboolean(var=value,varname='value',ivar=3)
    this$myterminateflag <- value
    if (!is.logical(value)){
      unknownValueForOption(value=value,optionname='-myterminateflag')
    } else {
      this$myterminateflag <- value
    }
  }
  if (key=='-tolvarianceflag'){
    assert.typeboolean(var=value,varname='value',ivar=3)
    this$tolvarianceflag <- value
  }
  if (key=='-tolabsolutevariance'){
    assert.typereal(var=value,varname='value',ivar=3)
    this$tolabsolutevariance <- value
  }
  if (key=='-tolrelativevariance'){
    assert.typereal(var=value,varname='value',ivar=3)
    this$tolrelativevariance <- value
  }
  if (key=='-greedy'){
    assert.typeboolean(var=value,varname='value',ivar=3)
    this$greedy <- value
  }

  return(this)
}


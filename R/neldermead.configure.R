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

neldermead.configure <- function(this=NULL,
                                 key=NULL,
                                 value=NULL){

  if (!any(key==c('-optbase','-method','-simplex0','-simplex0method',
      '-simplex0length','-simplexsize0','-simplexopt','-historysimplex',
      '-coords0','-rho','-chi','-','-gamma','-sigma','-tolfstdeviation',
      '-tolfstdeviationmethod','-tolsimplexizeabsolute',
      '-tolsimplexizerelative','-tolsimplexizemethod','-toldeltafv',
      '-tolssizedeltafvmethod','-simplex0deltausual','-simplex0deltazero',
      '-restartsimplexmethod','-restartmax','-restarteps','-restartstep',
      '-restartnb','-restartflag','-restartdetection','-kelleystagnationflag',
      '-kelleynormalizationflag','-kelleystagnationalpha0','-kelleyalpha',
      '-startupflag','-boxnbpoints','-boxnbpointseff','-boxineqscaling',
      '-checkcostfunction','-scalingsimplex0','-guinalphamin','-boxboundsalpha',
      '-boxtermination','-boxtolf','-boxnbmatch','-boxkount','-boxreflect',
      '-tolvarianceflag','-tolabsolutevariance','-tolrelativevariance',
      '-variancesimplex0','-mymethod','-myterminate','-myterminateflag',
      '-greedy','-output','-exitflag'))){
    # Delegate to the optimbase object
    this$optbase <- optimbase.configure(this=this$optbase,key=key,value=value)
  }

  if (key=='-optbase'){
    if (is(value)!='optimbase'){
      unknownValueForOption(value=value,optionname='-optbase')
    } else {
      this$optbase <- value
    }
    
  }
  if (key=='-method'){
    assert.classstring(var=value,varname='-method',ivar=3)
    if (!any(value==c('fixed','variable','box','mine'))){
      unknownValueForOption(value=value,optionname='-method')
    } else {
      this$method <- value
    }
  }
  if (key=='-simplex0'){
    if (is(value)!='simplex'){
      unknownValueForOption(value=value,optionname='-simplex0')
    } else {
      this$simplex0 <- value
    }
  }
  if (key=='-simplex0method'){
    assert.classstring(var=value,varname='-simplex0method',ivar=3)
    if (!any(value==c('given','axes','spendley','pfeffer','randbounds'))){
      unknownValueForOption(value=value,optionname='-simplex0method')
    } else {
      this$simplex0method <- value
    }
  }
  if (key=='-simplex0length'){
    assert.classreal(var=value,varname='-simplex0length',ivar=3)
    this$simplex0length <- value
  }
  if (key=='-simplexsize0'){
    assert.classreal(var=value,varname='simplexsize0',ivar=3)
    this$simplexsize0 <- value
  }
  if (key=='-simplexopt'){
    if (is(value)!='simplex'){
      unknownValueForOption(value=value,optionname='-simplexopt')
    } else {
      this$simplexopt <- value
    }
  }
  if (key=='-historysimplex'){
    if (is(value)!='list'){
      unknownValueForOption(value=value,optionname='-historysimplex')
    } else {
      this$historysimplex <- value
    }
  }
  if (key=='-coords0'){
    assert.classreal(var=value,varname='-coords0',ivar=3)
    this$coords0 <- value
  }
  if (key=='-rho'){
    assert.classreal(var=value,varname='-rho',ivar=3)
    this$rho <- value
  }
  if (key=='-chi'){
    assert.classreal(var=value,varname='-chi',ivar=3)
    this$chi <- value
  }
  if (key=='-gamma'){
    assert.classreal(var=value,varname='-gamma',ivar=3)
    this$gamma <- value
  }
  if (key=='-sigma'){
    assert.classreal(var=value,varname='-sigma',ivar=3)
    this$sigma <- value
  }
  if (key=='-tolfstdeviation'){
    assert.classreal(var=value,varname='-tolfstdeviation',ivar=3)
    this$tolfstdeviation <- value
  }
  if (key=='-tolfstdeviationmethod'){
    assert.classboolean(var=value,varname='-tolfstdeviationmethod',ivar=3)
    this$tolfstdeviationmethod <- value
  }
  if (key=='-tolsimplexizeabsolute'){
    assert.classreal(var=value,varname='-tolsimplexizeabsolute',ivar=3)
    this$tolsimplexizeabsolute <- value
  }
  if (key=='-tolsimplexizerelative'){
    assert.classreal(var=value,varname='-tolsimplexizerelative',ivar=3)
    this$tolsimplexizerelative <- value
  }
  if (key=='-tolsimplexizemethod'){
    assert.classboolean(var=value,varname='-tolsimplexizemethod',ivar=3)
    this$tolsimplexizemethod <- value
  }
  if (key=='-toldeltafv'){
    assert.classreal(var=value,varname='-toldeltafv',ivar=3)
    this$toldeltafv <- value
  }
  if (key=='-tolssizedeltafvmethod'){
    assert.classboolean(var=value,varname='-tolssizedeltafvmethod',ivar=3)
    this$tolssizedeltafvmethod <- value
  }
  if (key=='-simplex0deltausual'){
    assert.classreal(var=value,varname='-simplex0deltausual',ivar=3)
    this$simplex0deltausual <- value
  }
  if (key=='-simplex0deltazero'){
    assert.classreal(var=value,varname='-simplex0deltazero',ivar=3)
    this$simplex0deltazero <- value
  }
  if (key=='-restartsimplexmethod'){
    assert.classstring(var=value,varname='-restartsimplexmethod',ivar=3)
    this$restartsimplexmethod <- value
  }
  if (key=='-restartmax'){
    assert.classreal(var=value,varname='-restartmax',ivar=3)
    this$restartmax <- value
  }
  if (key=='-restarteps'){
    assert.classreal(var=value,varname='-restarteps',ivar=3)
    this$restarteps <- value
  }
  if (key=='-restartstep'){
    assert.classreal(var=value,varname='-restartstep',ivar=3)
    this$restartstep <- value
  }
  if (key=='-restartnb'){
    assert.classreal(var=value,varname='-restartnb',ivar=3)
    this$restartnb <- value
  }
  if (key=='-restartflag'){
    assert.classboolean(var=value,varname='-restartflag',ivar=3)
    this$restartflag <- value
  }
  if (key=='-restartdetection'){
    assert.classstring(var=value,varname='-restartdetection',ivar=3)
    this$restartdetection <- value
  }
  if (key=='-kelleystagnationflag'){
    assert.classboolean(var=value,varname='-kelleystagnationflag',ivar=3)
    this$kelleystagnationflag <- value
  }
  if (key=='-kelleynormalizationflag'){
    assert.classboolean(var=value,varname='-kelleynormalizationflag',ivar=3)
    this$kelleynormalizationflag <- value
  }
  if (key=='-kelleystagnationalpha0'){
    assert.classreal(var=value,varname='-kelleystagnationalpha0',ivar=3)
    this$kelleystagnationalpha0 <- value
  }
  if (key=='-kelleyalpha'){
    assert.classreal(var=value,varname='-kelleyalpha',ivar=3)
    this$kelleyalpha <- value
  }
  if (key=='-startupflag'){
    assert.classboolean(var=value,varname='-startupflag',ivar=3)
    this$startupflag <- value
  }
  if (key=='-boxnbpoints'){
    #assert.classstring(var=value,varname='-boxnbpoints',ivar=3)
    this$boxnbpoints <- value
  }
  if (key=='-boxnbpointseff'){
    assert.classreal(var=value,varname='-boxnbpointseff',ivar=3)
    this$boxnbpointseff <- value
  }
  if (key=='-boxineqscaling'){
    assert.classreal(var=value,varname='-boxineqscaling',ivar=3)
    this$boxineqscaling <- value
  }
  if (key=='-checkcostfunction') {
    assert.classboolean(var=value,varname='-checkcostfunction',ivar=3)
    this$checkcostfunction <- value
  }
  if (key=='-scalingsimplex0'){
    assert.classstring(var=value,varname='-scalingsimplex0',ivar=3)
    this$scalingsimplex0 <- value
  }
  if (key=='-guinalphamin'){
    assert.classreal(var=value,varname='-guinalphamin',ivar=3)
    if (value<=0)
      stop(sprintf('neldermead.configure: Unexpected negative value %s for -guinalphamin.',value),
           call.=FALSE)
    this$guinalphamin <- value
  }
  if (key=='-boxboundsalpha'){
    assert.classreal(var=value,varname='-boxboundsalpha',ivar=3)
    this$boxboundsalpha <- value
  }
  if (key=='-boxtermination'){
    assert.classboolean(var=value,varname='-boxtermination',ivar=3)
    this$boxtermination <- value
  }
  if (key=='-boxtolf'){
    assert.classreal(var=value,varname='-boxtolf',ivar=3)
    this$boxtolf <- value
  }
  if (key=='-boxnbmatch'){
    assert.classreal(var=value,varname='-boxnbmatch',ivar=3)
    this$boxnbmatch <- value
  }
  if (key=='-boxkount'){
    assert.classreal(var=value,varname='-boxkount',ivar=3)
    this$boxkount <- value
  }
  if (key=='-boxreflect'){
    assert.classreal(var=value,varname='-boxreflect',ivar=3)
    this$boxreflect <- value
  }
  if (key=='-tolvarianceflag'){
    assert.classboolean(var=value,varname='-tolvarianceflag',ivar=3)
    this$tolvarianceflag <- value
  }
  if (key=='-tolabsolutevariance'){
    assert.classreal(var=value,varname='-tolabsolutevariance',ivar=3)
    this$tolabsolutevariance <- value
  }
  if (key=='-tolrelativevariance'){
    assert.classreal(var=value,varname='-tolrelativevariance',ivar=3)
    this$tolrelativevariance <- value
  }
  if (key=='-variancesimplex0'){
    assert.classreal(var=value,varname='-variancesimplex0',ivar=3)
    this$variancesimplex0 <- value
  }
  if (key=='-mymethod'){
    assert.classfunction(var=value,varname='-mymethod',ivar=3)
    this$mymethod <- value
  }
  if (key=='-myterminate'){
    assert.classfunction(var=value,varname='-myterminate',ivar=3)
    this$myterminate <- value
  }
  if (key=='-myterminateflag'){
    assert.classboolean(var=value,varname='-myterminateflag',ivar=3)
    this$myterminateflag <- value
    if (!is.logical(value)){
      unknownValueForOption(value=value,optionname='-myterminateflag')
    } else {
      this$myterminateflag <- value
    }
  }
  if (key=='-greedy'){
    assert.classboolean(var=value,varname='-greedy',ivar=3)
    this$greedy <- value
  }
  if (key=='-output'){
    if (is(value)!='list'){
      unknownValueForOption(value=value,optionname='-output')
    } else {
      this$output <- value
    }
  }
  if (key=='-exitflag'){
    assert.classboolean(var=value,varname='-exitflag',ivar=3)
    this$exitflag <- value
  }
  return(this)
}


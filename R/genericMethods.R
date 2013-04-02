# genericMethods - «Short one line description»
# genericMethods

# Copyright 2009 Iago Mosqueira, JRC. Distributed under the GPL 2 or later
# $Id:  $

# Reference:
# Notes:

# FLMetier
setGeneric('FLMetier', function(catches, ...)
		standardGeneric('FLMetier'))

# gear
  setGeneric('gear', function(object, ...) standardGeneric('gear'))
  setGeneric('gear<-', function(object, ..., value) standardGeneric('gear<-'))

# effshare
  setGeneric('effshare', function(object, ...) standardGeneric('effshare'))
  setGeneric('effshare<-', function(object, ..., value) standardGeneric('effshare<-'))

# vcost
  setGeneric('vcost', function(object, ...) standardGeneric('vcost'))
  setGeneric('vcost<-', function(object, ..., value) standardGeneric('vcost<-'))

# catches
  setGeneric('catches', function(object, ...) standardGeneric('catches'))
#  setGeneric('catches<-', function(object, ..., value) standardGeneric('catches<-'))

# effort
  setGeneric("effort", function(object, metier, ...) standardGeneric("effort"))
  setGeneric("effort<-", function(object, ..., value) standardGeneric("effort<-"))

# fcost
  setGeneric('fcost', function(object, ...) standardGeneric('fcost'))
  setGeneric('fcost<-', function(object, ..., value) standardGeneric('fcost<-'))

# capacity
  setGeneric('capacity', function(object, ...) standardGeneric('capacity'))
  setGeneric('capacity<-', function(object, ..., value) standardGeneric('capacity<-'))

# crewshare
  setGeneric('crewshare', function(object, ...) standardGeneric('crewshare'))
  setGeneric('crewshare<-', function(object, ..., value) standardGeneric('crewshare<-'))

# metiers
  setGeneric('metiers', function(object, ...) standardGeneric('metiers'))
  setGeneric('metiers<-', function(object, ..., value) standardGeneric('metiers<-'))

# FLFleet
setGeneric('FLFleet', function(object, ...)
		standardGeneric('FLFleet'))

# metier
setGeneric('metier', function(object, metier, ...)
		standardGeneric('metier'))
setGeneric('metier<-', function(object, metier, ..., value)
		standardGeneric('metier<-'))

# FLCatch
setGeneric('FLCatch', function(object, ...)
		standardGeneric('FLCatch'))

# addFLCatch
setGeneric('addFLCatch', function(e1, e2, ...)
		standardGeneric('addFLCatch'))

# catchNames
setGeneric('catchNames', function(object, ...)
		standardGeneric('catchNames'))
setGeneric('catchNames<-', function(object, ..., value)
		standardGeneric('catchNames<-'))

# catch.sel
setGeneric('catch.sel', function(object, ...)
		standardGeneric('catch.sel'))

# FLCatches
setGeneric("FLCatches", function(object, ...)
	standardGeneric("FLCatches"))

# FLMetiers
setGeneric("FLMetiers", function(object, ...)
	standardGeneric("FLMetiers"))

# FLFleets
setGeneric("FLFleets", function(object, ...)
	standardGeneric("FLFleets"))

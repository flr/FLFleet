# classes.R - 
# FLFleet/R/classes.R

# Copyright 2003-2012 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

# FLCatch               {{{
validFLCatch <- function(object)
{
	names <- names(getSlots('FLCatch')[getSlots('FLCatch')=="FLQuant"])
  nits  <- sort(unique(unlist(qapply(object, function(x) dims(x)$iter))))
  
  if (length(nits)>2)
		return(paste("All FLQuant must either have same number of iters or '1 & n'"))

	for(i in names)
	{
		# all dimnames but iter are the same
		if(!identical(unlist(dimnames(object@landings.n)[2:5]),
			unlist(dimnames(slot(object, i))[2:5])))
			return(paste('All elements must share dimensions 2 to 5: Error in FLCatch', i))
	}
	for (i in names[!names%in%c('landings', 'discards', 'catch.q')])
	{
		# quant is n
		if(!identical(unlist(dimnames(object@landings.n)[1]),
			unlist(dimnames(slot(object, i))[1])))
			return(paste('All elements must share quant names: Error in FLCatch', i))
	}
	for (i in c('landings', 'discards'))
	{
		# quant is 1
		if(dim(slot(object, i))[1] != 1)
			return(paste('Wrong dimensions for slot ', i, 'in FLCatch'))
	}
	return(TRUE)
}
setClass("FLCatch",
    representation(
		'FLComp',
      landings    = "FLQuant", landings.n = "FLQuant",
		  landings.wt = "FLQuant", landings.sel = "FLQuant",
      discards    = "FLQuant", discards.n = "FLQuant",
      discards.wt = "FLQuant", discards.sel= "FLQuant",
		  catch.q = "FLQuant", price       = "FLQuant"),
    prototype=prototype(
		name		= character(0),
		desc		= character(0),
	  range       = as.numeric(c(min=NA, max=NA, plusgroup=NA,
			minyear=NA, maxyear=NA)),
    landings = new("FLQuant"), landings.n = new("FLQuant"),
    landings.wt = new("FLQuant"), landings.sel = new("FLQuant"),
    discards = new("FLQuant"), discards.n  = new("FLQuant"),
    discards.wt = new("FLQuant"), discards.sel= new("FLQuant"),
    catch.q     = new("FLQuant"), price = new("FLQuant")),
	validity=validFLCatch
)
remove(validFLCatch) # }}}

# FLCatches {{{
vFLSs <- function(object){
	
  # All items are FLCatch
  if(!all(unlist(lapply(object, is, 'FLCatch'))))
      return("Components must be FLCatch")	
	
	return(TRUE)
}

# class
setClass("FLCatches", contains="FLComps",
	validity=vFLSs
)

# constructor
setMethod("FLCatches", signature(object="FLCatch"), function(object, ...) {
    lst <- c(object, list(...))
    FLCatches(lst)
})

setMethod("FLCatches", signature(object="missing"),
  function(...) {
    # empty
  	if(missing(...)){
	  	new("FLCatches")
    # or not
  	} else {
      args <- list(...)
      object <- args[!names(args)%in%c('names', 'desc', 'lock')]
      args <- args[!names(args)%in%names(object)]
      do.call('FLCatches',  c(list(object=object), args))
	  }
  }
)

setMethod("FLCatches", signature(object="list"),
  function(object, ...) {
    
    args <- list(...)
    
    # names in args, ... 
    if("names" %in% names(args)) {
      names <- args[['names']]
    } else {
    # ... or in object,
      if(!is.null(names(object))) {
        names <- names(object)
    # ... or in elements, ...
      } else {
        names <- unlist(lapply(object, name))
        # ... or 1:n
        idx <- names == "NA" | names == ""
        if(any(idx))
          names[idx] <- as.character(length(names))[idx]
      }
    }

    # desc & lock
    args <- c(list(Class="FLCatches", .Data=object, names=names),
      args[!names(args)%in%'names'])

    return(
      do.call('new', args)
      )

}) # }}}

# FLMetier		{{{
validFLMetier <- function(object) {
	# FLQuant slots share dims 1:5 ...
  dnames <- qapply(object, function(x) dimnames(x)[3:5])
	for(i in names(dnames))
		if(!identical(dnames[[i]], dnames[[1]]))
			return('All FLQuant slots must have the same dimensions')

  # ... and are consistent with catches
  catdnames <- lapply(object@catches, function(x)
    qapply(object, function(x) dimnames(x)[3:5]))
  for(i in seq(length=length(catdnames)))
    for(j in names(catdnames[[1]]))
	    if(!identical(catdnames[[i]][[j]], dnames[[1]]))
			  return('All FLQuant slots must have the same dimensions')
  
  # Year range of FLMetier covers all catches
  catyears <- matrix(unlist(lapply(object@catches, function(x) 
    unlist(dims(x)[c('minyear', 'maxyear')]))), byrow=TRUE, ncol=2)
  if(any(dims(object)$minyear < catyears [,1]) |
    any(dims(object)$maxyear > catyears [,2]))
    return('Year range of metier should encompass those of catch(es)')

  # iter is consistent between metier and catches
  if(any(dims(object)$iter != unlist(lapply(object@catches, function(x) dims(x)$iter))))
    return('iter must be 1 or N across all slots and levels')

	return(TRUE)
}

setClass('FLMetier',
	representation('FLComp',
		gear='character',
		effshare='FLQuant',
		vcost='FLQuant',
		catches='FLCatches'),
	prototype(name=character(0), desc=character(0),
		range= unlist(list(min=0, max=0, plusgroup=NA, minyear=1, maxyear=1)),
		gear=character(0), catches=new('FLCatches'), effshare=FLQuant(1), vcost=FLQuant(NA)),
	validity=validFLMetier)

remove(validFLMetier)
# Accesors
createFLAccesors('FLMetier', exclude=c('range', 'catches', 'name', 'desc'))
# }}}

# FLMetiers {{{
vFLSs <- function(object){
	
  # All items are FLMetier
  if(!all(unlist(lapply(object, is, 'FLMetier'))))
      return("Components must be FLMetier")	
	
	return(TRUE)
}

# class
setClass("FLMetiers", contains="FLComps",
	validity=vFLSs
)

# constructor
setMethod("FLMetiers", signature(object="FLMetier"), function(object, ...) {
    lst <- c(object, list(...))
    FLMetiers(lst)
})

setMethod("FLMetiers", signature(object="missing"),
  function(...) {
    # empty
  	if(missing(...)){
	  	new("FLMetiers")
    # or not
  	} else {
      args <- list(...)
      object <- args[!names(args)%in%c('names', 'desc', 'lock')]
      args <- args[!names(args)%in%names(object)]
      do.call('FLMetiers',  c(list(object=object), args))
	  }
  }
)

setMethod("FLMetiers", signature(object="list"),
  function(object, ...) {
    
    args <- list(...)
    
    # names in args, ... 
    if("names" %in% names(args)) {
      names <- args[['names']]
    } else {
    # ... or in object,
      if(!is.null(names(object))) {
        names <- names(object)
    # ... or in elements, ...
      } else {
        names <- unlist(lapply(object, name))
        # ... or 1:n
        idx <- names == "NA" | names == ""
        if(any(idx))
          names[idx] <- as.character(length(names))[idx]
      }
    }

    # desc & lock
    args <- c(list(Class="FLMetiers", .Data=object, names=names),
      args[!names(args)%in%'names'])

    return(
      do.call('new', args)
      )

}) # }}}

# FLFleet		{{{
validFLFleet <- function(object) {

	# FLQuant slots share dims 3:5 ...
  dnames <- qapply(object, function(x) dimnames(x)[3:5])
	for(i in names(dnames))
		if(!identical(dnames[[i]], dnames[[1]]))
			return('All FLQuant slots must have the same dimensions')

  # ... and are consistent with metiers
  metdnames <- lapply(object@metiers, function(x)
    qapply(object, function(x) dimnames(x)[3:5]))
  for(i in seq(length=length(metdnames)))
    for(j in names(metdnames[[1]]))
	    if(!identical(metdnames[[i]][[j]], dnames[[1]]))
			  return('All FLQuant slots must have the same dimensions')
  
  # Year range of FLFleet covers all metiers
  metyears <- matrix(unlist(lapply(object@metiers, function(x) 
    unlist(dims(x)[c('minyear', 'maxyear')]))), byrow=TRUE, ncol=2)

  if(any(dims(object)$minyear < metyears [,1]) |
    any(dims(object)$maxyear > metyears [,2]))
    return('Year range of fleet should encompass those of metier(s)')

  # iter is consistent between fleet and metiers
  if(any(dims(object)$iter != unlist(lapply(object@metiers, function(x) dims(x)$iter))))
    return('iter must be 1 or N across all slots and levels')

  # effshares must add up to one
  #effshs <- lapply(object@metiers, effshare)
  #if(length(effshs) > 1)
  #  for(i in 2:length(effshs))
  #    effshs[[1]] <- effshs[[1]] + effshs[[i]]
  #if(!isTRUE(all.equal(as.vector(effshs[[1]]), rep(1,prod(dim(effshs[[1]]))))))
  #  return('sum of effshare must add up to 1')

	return(TRUE)
}

setClass('FLFleet',
	representation('FLComp',
		effort='FLQuant',
		fcost='FLQuant',
		capacity='FLQuant',
		crewshare ="FLQuant",
		metiers='FLMetiers'),
	prototype(name=character(0), desc=character(0),
		range= unlist(list(min=0, max=0, plusgroup=NA, minyear=1, maxyear=1)),
		effort=FLQuant(), fcost=FLQuant(), capacity=FLQuant(),
		crewshare=FLQuant(), metiers=FLMetiers()),
	validity=validFLFleet)
remove(validFLFleet)

invisible(createFLAccesors("FLFleet", exclude=c('range', 'effort', 'name', 'desc')))	# }}}

# FLFleets {{{
vFLSs <- function(object){
	
  # All items are FLFleet
  if(!all(unlist(lapply(object, is, 'FLFleet'))))
      return("Components must be FLFleet")	
	
	return(TRUE)
}

# class
setClass("FLFleets", contains="FLComps",
	validity=vFLSs
)

# constructor
setMethod("FLFleets", signature(object="FLFleet"), function(object, ...) {
    lst <- c(object, list(...))
    FLFleets(lst)
})

setMethod("FLFleets", signature(object="missing"),
  function(...) {
    # empty
  	if(missing(...)){
	  	new("FLFleets")
    # or not
  	} else {
      args <- list(...)
      object <- args[!names(args)%in%c('names', 'desc', 'lock')]
      args <- args[!names(args)%in%names(object)]
      do.call('FLFleets',  c(list(object=object), args))
	  }
  }
)

setMethod("FLFleets", signature(object="list"),
  function(object, ...) {
    
    args <- list(...)
    
    # names in args, ... 
    if("names" %in% names(args)) {
      names <- args[['names']]
    } else {
    # ... or in object,
      if(!is.null(names(object))) {
        names <- names(object)
    # ... or in elements, ...
      } else {
        names <- unlist(lapply(object, name))
        # ... or 1:n
        idx <- names == "NA" | names == ""
        if(any(idx))
          names[idx] <- as.character(length(names))[idx]
      }
    }

    # desc & lock
    args <- c(list(Class="FLFleets", .Data=object, names=names),
      args[!names(args)%in%'names'])

    return(
      do.call('new', args)
      )

}) # }}}

# FLFleet - FLFleet class and methods
# FLCore/R/FLFleet.R

# Copyright 2003-2012 FLR Team. Distributed under the GPL 2 or later
# Maintainer:
# $Id: FLFleet.R 1778 2012-11-23 08:43:57Z imosqueira $


# FLFleet()		{{{
setMethod('FLFleet', signature(object='FLMetiers'),
	function(object, ...)
	{
		args <- list(...)
		flqs <- unlist(lapply(args, is, 'FLQuant'))
		if(any(flqs))
			flqs <- FLQuant(NA,
				dimnames=c(dimnames(args[[names(flqs[flqs==TRUE])[1]]])[-6], list(iter=1)))
		else
			flqs <- FLQuant()
		res <- new('FLFleet', metiers=object, effort=flqs, fcost=flqs,
			capacity=flqs, crewshare=flqs, range=range(object))

		# extra arguments
		for (i in names(args))
			slot(res, i) <- args[[i]]
		return(res)
	}
)
setMethod('FLFleet', signature(object='FLMetier'),
	function(object, ...)
	{
		FLFleet(FLMetiers(met=object), ...)
	}
)
setMethod('FLFleet', signature(object='FLCatches'),
	function(object, ...)
	{
		FLFleet(FLMetiers(FLMetier(object)), ...)
	}
)
setMethod('FLFleet', signature(object='FLCatch'),
	function(object, ...)
	{
		FLFleet(FLMetiers(FLMetier(FLCatches(object))), ...)
	}
)
setMethod('FLFleet', signature(object='FLFleet'),
	function(object, metier, catch, ...)
	{
    stop("TODO")
	}
)
setMethod('FLFleet', signature(object='missing'),
	function(object, ...)
	{
		FLFleet(FLMetiers(FLMetier(FLCatches(FLCatch()))), ...)
	}
)	# }}}

# summary	{{{
setMethod('summary', signature(object='FLFleet'),
	function(object, ...)
	{
		callNextMethod(object)
		cat("\n")
		cat("Metiers: ", "\n")
		# TODO What happens when object has no metiers/catches? IM 28.08.07
		for (i in names(object@metiers))
		{
			cat("\t", i, ":\n")
			
			for (j in names(object@metiers[[i]]@catches))
				cat("\t\t", j, ": [", dim(object@metiers[[i]]@catches[[j]]@landings.n),"]\n")
		}
	}
)
# }}}

# metier(fl, me)	{{{
setMethod('metier', signature(object='FLFleet', metier='ANY'),
	function(object, metier, ...)
		return(object@metiers[[metier]])
)
setReplaceMethod('metier', signature(object='FLFleet', metier='ANY', value='FLMetier'),
	function(object, metier, ..., value)
	{
		object@metiers[[metier]] <- value
		return(object)
	}
)	# }}}

# FLFleet accesors	{{{
createFleetAccesors('catch', catch, c(2:5), assigment=FALSE)
createFleetAccesors('catch.n', catch.n, c(2:5), assigment=FALSE)
createFleetAccesors('catch.wt', catch.wt, c(2:5), assigment=FALSE)
createFleetAccesors('catch.sel', catch.sel, c(2:5), assigment=FALSE)
createFleetAccesors('catch.q', catch.q)
createFleetAccesors('discards', discards)
createFleetAccesors('discards.n', discards.n)
createFleetAccesors('discards.wt', discards.wt)
createFleetAccesors('discards.sel', discards.sel)
createFleetAccesors('landings', landings)
createFleetAccesors('landings.n', landings.n)
createFleetAccesors('landings.wt', landings.wt)
createFleetAccesors('landings.sel', landings.sel)
createFleetAccesors('price', price)
# }}}

## iter {{{
setMethod("iter", signature(obj="FLFleet"),
	  function(obj, iter)
	  {
		# FLQuant slots
		names <- names(getSlots(class(obj))[getSlots(class(obj))=="FLQuant"])
		for(s in names) 
		{
			if(dims(slot(obj, s))$iter == 1)
				slot(obj, s) <- iter(slot(obj, s), 1)
			else
				slot(obj, s) <- iter(slot(obj, s), iter)
		}
		# FLMetiers
		names <- names(obj@metiers)
		for (s in names)
			metier(obj, s) <- iter(metier(obj, s), iter)
		
		return(obj)
	  }
) # }}}

# catches(fl, me, ca)	{{{
setMethod('catches', signature(object='FLFleet'),
	function(object, ...)
		return(catches(object@metiers, ...))
)
setMethod('catches', signature(object='FLMetiers'),
	function(object, catch='missing', sum=FALSE, ...)
  {
    # No catch? OK if only one in object
    if(missing(catch))
      if(length(unique(unlist(lapply(object, function(x) names(x@catches))))) == 1)
        catch <- object[[1]]@catches[[1]]@name
      else
        stop('No catch was selected and object holds data for more than one catch')
    
    # identify metiers with this catch.
    idx <- unlist(lapply(object, function(x) any(catchNames(x) == catch)))

    # if index is numeric and only one metier, select from names
    if(length(object) == 1 & is.numeric(catch))
      catch <- catchNames(object)[catch]
    res <- lapply(object[idx], catches, catch=catch)
    
    if(length(res) > 1 && sum==TRUE)
    {
      res <- mcf(res)
      res[[1]] <- res[[1]] + res[[2]]
      if(length(res) > 2)
        for(i in seq(3, length(res)))
        {
          res[[1]] <- addFLCatch(res[[1]], res[[i]])
        }
      return(FLCatches(res[[1]]))
    }
    return(FLCatches(res))
  }
)
setMethod('catches', signature(object='FLMetier'),
	function(object, catch='missing', ...)
  {
		if(missing(catch))
      return(object@catches)
    if (length(catch) == 1)
      return(object@catches[[catch]])
    else
      return(object@catches[catch])
  }
)	# }}}

# catches<-(fl, ca)	{{{
setMethod('catches<-', signature(object='FLMetier', value='FLCatch'),
	function(object, catch, ..., value)
  {
    object@catches[[catch]] <- value
    return(object)
  }
)
setMethod('catches<-', signature(object='FLMetier', value='FLCatches'),
	function(object, catch, ..., value)
  {
    object@catches <- value
    return(object)
  }
) # }}}

# FLMetier accesors for FLFleet {{{
setMethod('effshare', signature(object='FLMetiers'),
  function(object, metier=names(object))
  {
    if(length(metier) == 1)
      return(object[[metier]]@effshare)
    else
      return(FLQuants(lapply(object[metier], effshare)))
  }
)
setMethod('effshare', signature(object='FLFleet'),
  function(object, ...)
    return(effshare(object@metiers, ...))
)
setMethod('vcost', signature(object='FLMetiers'),
  function(object, metier=names(object))
  {
    if(length(metier) == 1)
      return(object[[metier]]@vcost)
    else
      return(FLQuants(lapply(object[metier], vcost)))
  }
)
setMethod('vcost', signature(object='FLFleet'),
  function(object, ...)
    return(vcost(object@metiers, ...))
)
# }}}

## dims {{{
setMethod("dims", signature(obj="FLFleet"),
  # Returns a list with different parameters
  function(obj, ...)
	{
		qnames <- names(getSlots(class(obj))[getSlots(class(obj))=="FLQuant"])
		return(list(
      metiers=names(obj@metiers),
      catches=unique(unlist(lapply(obj@metiers, function(x) names(x@catches)))),
      quant = quant(slot(obj, qnames[1])),
      min=min(as.numeric(unlist(lapply(obj@metiers, function(x) lapply(x@catches,
        function(x) dimnames(x@landings.n)[[1]][1]))))),
      max=max(as.numeric(unlist(lapply(obj@metiers, function(x) lapply(x@catches,
        function(x) dimnames(x@landings.n)[[1]][dim(x@landings.n)[1]]))))),
      minyear=min(as.numeric(unlist(lapply(obj@metiers, function(x) lapply(x@catches,
        function(x) dimnames(x@landings.n)[[2]][1]))))),
      maxyear=max(as.numeric(unlist(lapply(obj@metiers, function(x) lapply(x@catches,
        function(x) dimnames(x@landings.n)[[2]][dim(x@landings.n)[2]]))))),
      unit=unique(unlist(lapply(obj@metiers, function(x) lapply(x@catches,
        function(x) length(dimnames(x@landings.n)[[3]]))))),
      season=unique(unlist(lapply(obj@metiers, function(x) lapply(x@catches,
        function(x) length(dimnames(x@landings.n)[[4]]))))),
      area=unique(unlist(lapply(obj@metiers, function(x) lapply(x@catches,
        function(x) length(dimnames(x@landings.n)[[5]]))))),
      iter=max(unlist(lapply(obj@metiers, function(x) lapply(x@catches,
        function(x) qapply(x, function(x) length(dimnames(x)[[6]]))))))
    ))
    }
)    # }}}

## window    {{{
setMethod("window", signature(x="FLFleet"),
	  function(x, start=dims(x)$minyear, end=dims(x)$maxyear, extend=TRUE, frequency=1) {

    # window fleet
    x <- qapply(x, window, start, end, extend, frequency)

    # window metiers
    x@metiers <- lapply(x@metiers, window, start, end, extend, frequency)

    # window catches
    for(i in seq(length(x@metiers)))
      x@metiers[[i]]@catches <- lapply(x@metiers[[i]]@catches, window, start, end, extend, frequency)

		x@range["minyear"] <- start
		x@range["maxyear"] <- end

		return(x)
	}
)	# }}}

## effort		{{{
setMethod("effort", signature(object="FLFleet", metier="missing"),
	function(object)
    return(slot(object, "effort")))

setMethod("effort", signature(object="FLFleet", metier="character"),
	function(object, metier)
    return(slot(object, "effort") * slot(metier(object, metier), "effshare")))

setReplaceMethod("effort", signature(object="FLFleet", value="FLQuant"),
	function(object, value)
  {
		slot(object, "effort") <- value
    return(object)
  })
# }}}

# catchNames {{{
setMethod('catchNames', signature(object='FLCatches'),
  function(object)
  {
    return(unname(unlist(lapply(object, catchNames))))
  }
)
setMethod('catchNames', signature(object='FLMetier'),
  function(object)
  {
    return(catchNames(object@catches))
  }
)
setMethod('catchNames', signature(object='FLMetiers'),
  function(object)
  {
    return(unique(unlist(lapply(object, catchNames))))
  }
)
setMethod('catchNames', signature(object='FLFleet'),
  function(object)
  {
    return(catchNames(object@metiers))
  }
) 
setMethod('catchNames', signature(object='FLFleets'),
  function(object)
  {
    return(unique(unlist(lapply(object, catchNames))))
  }
) # }}}

# trim {{{
setMethod('trim', signature(x='FLFleet'),
  function(x, ...)
  {
    x <- callNextMethod()
    x@metiers <- lapply(x@metiers, trim, ...)
    return(x)
  }
) # }}}

# propagate {{{
setMethod('propagate', signature(object='FLFleet'),
  function(object, ...)
  {
    object <- qapply(object, propagate, ...)
    object@metiers <- lapply(object@metiers, propagate, ...)
    return(object)
  }
) # }}}

# computeCatch  {{{
setMethod('computeCatch', signature(object='FLCatch'),
  function(object)
    return(quantSums(catch.n(object) * catch.wt(object)))
)
setMethod('computeDiscards', signature(object='FLCatch'),
  function(object)
    return(quantSums(discards.n(object) * discards.wt(object)))
)
setMethod('computeLandings', signature(object='FLCatch'),
  function(object)
    return(quantSums(landings.n(object) * landings.wt(object)))
)

setMethod('computeCatch', signature(object='FLMetier'),
  function(object, catch=names(object@catches))
  lapply(object@catches[catch], computeCatch)
)
setMethod('computeDiscards', signature(object='FLMetier'),
  function(object, catch=names(object@catches))
  lapply(object@catches[catch], computeDiscards)
)
setMethod('computeLandings', signature(object='FLMetier'),
  function(object, catch=names(object@catches))
  lapply(object@catches[catch], computeLandings)
)

setMethod('computeCatch', signature(object='FLFleet'),
  function(object, ...)
  lapply(object@metiers, computeCatch, ...)
)
setMethod('computeDiscards', signature(object='FLFleet'),
  function(object, ...)
  lapply(object@metiers, computeDiscards, ...)
)
setMethod('computeLandings', signature(object='FLFleet'),
  function(object, ...)
  lapply(object@metiers, computeLandings, ...)
) # }}}

# "[" and "[["             {{{
setMethod("[", signature(x="FLFleet", i="ANY", j="missing"),
  function(x, i, drop=FALSE)
  {
	  if (missing(i))
      return(x)
    x@metiers <- x@metiers[i]
    return(x)
	}
)

setMethod("[", signature(x="FLFleet", i="ANY", j="ANY"),
  function(x, i, j, drop=FALSE)
  {
    x <- x[i]
    if(!missing(j))
      x@metiers <- lapply(x@metiers, '[', j)
    return(x)
	}
)

setMethod("[", signature(x="FLFleet", i="missing", j="ANY"),
  function(x, i, j, drop=FALSE)
  {
    if(length(x@metiers) > 1)
      stop("No metier selected, but more than one in FLFleet object")
    else
      x@metiers <- lapply(x@metiers, '[', j)
    return(x)
	}
)

setMethod("[[", signature(x="FLFleet", i="character", j="missing"),
  function(x, i, drop=FALSE)
  {
    return(x@metiers[[i]])
	}
) 

setMethod("[[", signature(x="FLFleet", i="numeric", j="missing"),
  function(x, i, drop=FALSE)
  {
    return(x@metiers[[i]])
	}
) 
# }}}

# as.data.frame {{{
setMethod('as.data.frame', signature(x='FLFleet', row.names='missing',
  optional='missing'), function(x)
  {
    df <- callNextMethod()
    df <- cbind(df, metier='NA', catch='NA')

    for (i in 1:length(x@metiers))
    {
      df <- rbind(df, cbind(catch='NA', metier=names(x@metiers)[[i]],
        as.data.frame(x@metiers[[i]])))

      for (j in 1:length(x@metiers[[i]]@catches))
      df <- rbind(df, cbind(catch=names(x@metiers[[i]]@catches)[[j]],
        metier=names(x@metiers)[[i]], as.data.frame(x@metiers[[i]]@catches[[j]])))
    }
    return(df)
  }
) # }}}

## dims(FLFleets) {{{
setMethod("dims", signature(obj="FLFleets"),
  # Returns a list with different parameters
  function(obj, ...)
	{
		return(list(
      fleets=names(obj),
      metiers=unique(unlist(lapply(obj, function(x) names(x@metiers)))),
      catches=unique(unlist(lapply(obj, function(x) lapply(x@metiers, function(x) names(x@catches))))),
      quant = unlist(lapply(obj, function(x) quant(landings.n(x, 1, 1)))),
      min=min(unlist(lapply(obj, function(obj) min(as.numeric(unlist(lapply(obj@metiers, function(x) lapply(x@catches, function(x) dimnames(x@landings.n)[[1]][1])))))))),
      max=max(unlist(lapply(obj, function(obj) as.numeric(unlist(lapply(obj@metiers, function(x) lapply(x@catches, function(x) dimnames(x@landings.n)[[1]][dim(x@landings.n)[1]]))))))),
      minyear=min(unlist(lapply(obj, function(obj) as.numeric(unlist(lapply(obj@metiers, function(x) lapply(x@catches, function(x) dimnames(x@landings.n)[[2]][1]))))))),
      maxyear=max(unlist(lapply(obj, function(obj) as.numeric(unlist(lapply(obj@metiers, function(x) lapply(x@catches, function(x) dimnames(x@landings.n)[[2]][dim(x@landings.n)[2]]))))))),
      unit=unlist(lapply(obj, function(obj) unique(unlist(lapply(obj@metiers, function(x) lapply(x@catches, function(x) length(dimnames(x@landings.n)[[3]]))))))),
      season=unlist(lapply(obj, function(obj) unique(unlist(lapply(obj@metiers, function(x) lapply(x@catches, function(x) length(dimnames(x@landings.n)[[4]]))))))),
      area=unlist(lapply(obj, function(obj) unique(unlist(lapply(obj@metiers, function(x) lapply(x@catches, function(x) length(dimnames(x@landings.n)[[5]]))))))),
      iter=unlist(lapply(obj, function(x) max(unlist(lapply(x@metiers, function(x) lapply(x@catches, function(x) qapply(x, function(x) length(dimnames(x)[[6]]))))))))
    ))
    }
)    # }}}

# FLFleet -> FLFleets {{{
setMethod('getPlural', signature(object='FLFleet'),
	function(object) {
		return('FLFleets')}) # }}}

# tcost {{{
setMethod("tcost", signature(object='FLFleet'), function(object) {

	# fcost +
	return(fcost(object) +

	# vcost +
	Reduce('+', vcost(object)) +

	# ccost
	crewshare(object) * revenue(object))

}) # }}}

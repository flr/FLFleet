# methods.R - DESC
# methods.R

# Copyright 2015 Iago Mosqueira. Distributed under the GPL 2.
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

# revenue	{{{
setMethod('revenue', signature('FLCatch'),
	function(object) {
    if(!all(is.na(landings.n(object))))
			res <- quantSums(landings.n(object) * landings.wt(object) * price(object))
    else
      res <- landings(object) * price(object)
		# IF revenue is NA, then return as 0
		res[is.na(res)] <- 0
		return(res)
	}
)
setMethod('revenue', signature('FLCatches'),
	function(object, catch=unique(names(object)), ...)
		return(lapply(object, revenue))
)
setMethod('revenue', signature('FLMetier'),
  function(object, ...) {
		res <- Reduce('+', lapply(catches(object), revenue))
    return(res)
  }
)
setMethod('revenue', signature('FLMetiers'),
  function(object)
  {
		return(lapply(object, revenue))
  }
)
setMethod('revenue', signature('FLFleet'),
  function(object, ...)
  {
		res <- Reduce('+', lapply(metiers(object), revenue))
    return(res)
  }
) # }}}

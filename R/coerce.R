# coerce - Various coercion methods for FLCore classes
# FLCore/R/coerce.R

# Copyright 2003-2008 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: coerce.R 1789 2012-12-10 10:34:22Z imosqueira $

# From FLCatch	{{{
setAs('FLCatch', 'FLStock',
	function(from)
	{
		# FLCatch from FLStock
		res <- FLStock(landings.n=landings.n(from), landings.wt=landings.wt(from),
		landings=landings(from), discards.n=discards.n(from), discards.wt=discards.wt(from),
    discards=discards(from), catch=catch(from), catch.wt=catch.wt(from),
    catch.n=catch.n(from), name=name(from), range=from@range)

		return(res)
	}
)	# }}}

# FLMetier	{{{
	

setAs('FLCatch', 'FLMetier',
  function(from)
  {
    return(FLMetier(from, range=from@range))
  }
)

setAs('FLCatch', 'FLMetiers',
  function(from)
  {
    return(FLMetiers(FLMetier(from)))
  }
)
setAs('FLCatch', 'FLIndex',
  function(from)
	{
    dmns    <-dimnames(from@landings.n)
    dmns$age<-"all"
    
		res<-FLIndex(catch.n    =catch.n(from),
                 index      =FLQuant(NA, dimnames=dimnames(from@landings.n)),
                 index.var  =FLQuant(NA, dimnames=dimnames(from@landings.n)),
                 catch.wt   =catch.wt(from),
                 effort     =FLQuant(NA, dimnames=dmns),
                 sel.pattern=FLQuant(NA, dimnames=dimnames(from@landings.n)),
                 index.q    =FLQuant(1,  dimnames=dimnames(from@landings.n)),
                 range      =from@range,
                 type="number",
			           name=from@name, desc=paste("Coerced from FLCatch:",from@desc))
			           
    units(res@index)   <-"NA"
    units(res@catch.n) <-units(from@landings.n)
    units(res@catch.wt)<-units(from@landings.wt)
    
    res@range<-c(res@range,startf=0.0,endf=0.01)

  return(res)
	}
)
# }}}

# From FLStock  {{{
setAs('FLStock', 'FLCatch',
	function(from)
	{
		# FLCatch from FLStock
		res <- FLCatch(landings.n=landings.n(from), landings.wt=landings.wt(from),
		landings=landings(from), discards.n=discards.n(from),
		discards.wt=discards.wt(from), discards=discards(from),
    name=name(from), range=from@range, catch.q=FLQuant(1, dimnames=dimnames(catch(from))))

		# selectivities
    fbarAges <-as.character(from@range["minfbar"]:from@range["maxfbar"])
    catch.sel <- sweep(harvest(from), 2:6, apply(harvest(from)[fbarAges], 2:6, mean), "/")
    catch.ratio <- landings.n(from)/(landings.n(from)+discards.n(from))
    landings.sel(res) <- catch.sel * catch.ratio
   	discards.sel(res) <- catch.sel * (1-catch.ratio)

		return(res)
	}
) 

setAs('FLStock', 'FLFleet',
	function(from)
	  {
		# FLCatch from FLStock
		res <- as(from, 'FLCatch')
		return(FLFleet(res, range=from@range, effort=apply(harvest(from)[ac(range(from,"minfbar"):range(from,"maxfbar")),],c(2,4:6),mean)))
  	}
) 

setAs('FLStock', 'FLMetier',
	function(from)
	{
		# FLCatch from FLStock
		res <- as(from, 'FLCatch')
		return(FLMetier(res, range=from@range))
	}
)

# }}}

## as.FLIndex::FLFleet      {{{
setMethod("as.FLIndex", signature(object="FLFleet"),
    function(object, catchname="missing", catchtype="missing", ...) {
    
    # Check if valid fleet
    validObject(object)
    
    # If only one spp in @catch and spp=missing, take it
    if (missing(catchname) && length(object@catches)==1)

    indstock <- 1

    # If spp is character, look for it in @catch
    else if (is.character(catchname)) {
        if(length(object@catches) == 1) {
            if(object@catches[[1]]@name != catchname)
                stop(paste("Catchname ", catchname, "cannot be found in object"))
            else
                indstock <- 1
            } else {

            # get vector of spp in FLCatch
            indstock <- vector()
            for(i in seq(along=object@catches))
                if (object@catches[[i]]@name == catchname) indstock[i] <- i
                    indstock <- indstock[!is.na(indstock)]
                if (length(indstock)==0) stop(paste("Catchname ", catchname,
                    "cannot be found in fleet by as.FLIndex()"))
                if (length(indstock)>0)  stop(paste("More than 1 occurrence of ", catchname,
                    " found in fleet by as.FLIndex()"))
            }

        } else
            stop("stock must be a character string or a number")

        # Output FLIndex object
        # now, indstock contains the catches which need to be put in new Index object.
        # length of indstock==1, so simple copying of slots. If length >1 then calculations are needed

        if (length(indstock)==1){
            fli <- FLIndex(name=paste("catchtype ",
                ifelse((missing(catchtype)||catchtype == "catch"),"catch","landings"),
                "derived from FLfleet, with catchname " , catchname),
                iniFLQuant=object@catches[[indstock]]@landings.n)

            # range (20/5/2005; treated separately because plusgroup is missing in fleet@range
            fli@range["min"] <- object@catches[[indstock]]@range["min"]
            fli@range["max"] <- object@catches[[indstock]]@range["max"]
            fli@range["minyear"] <- object@catches[[indstock]]@range["minyear"]
            fli@range["maxyear"] <- object@catches[[indstock]]@range["maxyear"]
            fli@range["plusgroup"] <- object@catches[[indstock]]@range["max"]

            # q
            fli@index.q <- object@catches[[indstock]]@catchability

            # effort, catch
            fli@effort   <-  object@effort
            if (missing(catchtype) || catchtype == "catch") {
                fli@index    <-  object@catches[[indstock]]@catch.n
                fli@catch.wt <-  object@catches[[indstock]]@catch.wt
            } else if (catchtype == "landings") {
                fli@index    <-  object@catches[[indstock]]@landings.n
                fli@catch.wt <-  object@catches[[indstock]]@landings.wt
            } else
                stop(paste("Catchtype ", catchtype, " not recognized"))
        }
    return(fli)
    }
)   # }}}

setAs("NULL", "FLCatch", function(from) FLCatch())
setAs("NULL", "FLMetier", function(from) FLMetier())
setAs("NULL", "FLFleet", function(from) FLFleet())


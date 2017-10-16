
#' define objects
#' @param Qsb
#' @export
define_obj_Qsb<- function(spObj,timeObj)
{
    Qsb <- STFDF(spObj,time=timeObj,data=data.frame(Qin=NA,Qout=NA,V=NA))    
    return(Qsb)
}

#' define objects
#' @param Qpipe
#' @export
define_obj_Qpipe<- function(spObj)
{
    Qpipe <- SpatialPointsDataFrame(coords=spObj@coords,data.frame(Qin=NA,Qout=NA,V=NA),proj4string=spObj@proj4string)
    return(Qpipe)
}

#' define objects
#' @param Qret_str
#' @export
define_obj_Qret_str<- function(spObj)
{
    Qret_str <- SpatialPointsDataFrame(coords=spObj@coords,data.frame(Qin=NA,Qout=NA,Qover=NA,V=NA),proj4string=spObj@proj4string)
    return(Qret_str)
}


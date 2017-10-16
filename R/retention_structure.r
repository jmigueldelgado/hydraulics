#' Virtual volume in retention structure
#' @param Qin is a SpatialPointsDataFrame inflow to structure in step i
#' @param Qoverflow is a SpatialPointsDataFrame unregulated overflow from structure in step i
#' @param Vvirtual is the virtual volume in step i, including overflow volume
#' @param Vprevious is the volume in step i-1
#' @param param.df is a SpatialPointsDataFrame of parameters of each subbasin, including df (time step), Vmax (maximum allowable volume of structure) and Qoutmax (maximum regulated allowable outflow from structure)
#' @export
Virtual_retention <- function(Qin,Qoverflow,Vprevious,parm.df)
{
    Vvirtual <- (Qin-parm.df@data$Qoutmax)*parm.df@data$dt + Vprevious
    return(Vvirtual)
}

#' Actual volume in retention structure
#' @param Qin is a SpatialPointsDataFrame inflow to structure in step i
#' @param Qoverflow is a SpatialPointsDataFrame unregulated overflow from structure in step i
#' @param Vactual is the virtual volume in step i, including overflow volume
#' @param Vprevious is the volume in step i-1
#' @param param.df is a SpatialPointsDataFrame of parameters of each subbasin, including df (time step), Vmax (maximum allowable volume of structure) and Qoutmax (maximum regulated allowable outflow from structure)
#' @export
Actual_retention <- function(Qin,Qoverflow,Vprevious,parm.df)
{
    #'case 1 Vvirtual==0
    V <- 0

    #' case 2 Vvirtual>parm.df$Vmax
    V <- parm.df$Vmax
    #' case 3 Vvirtual >0 & Vvirtual< parm.df$Vmax
    V <- (Qin-parm.df$Qoutmax)*dt+Vprevious
    return(V)
}

#' Outflow from retention structure
#' @param Qin is a SpatialPointsDataFrame inflow to structure in step i
#' @param Qout is a SpatialPointsDataFrame regulated outflow from structure in step i
#' @param Vvirtual is the virtual volume in step i, including overflow volume
#' @param Vprevious is the volume in step i-1
#' @param param.df is a SpatialPointsDataFrame of parameters of each subbasin, including df (time step), Vmax (maximum allowable volume of structure) and Qoutmax (maximum regulated allowable outflow from structure)
#' @export
Qoutflow_ret_str <- function(Qin,Qout,Vprevious,Vvirtual,parm.df)
{
    #'case 1 Vvirtual==0
    V <- 0
    Qout <- Vprevious/dt + Qin

    #' case 2 Vvirtual>parm.df$Vmax
    Qout <- parm.df$Qoutmax
    
    #' case 3 Vvirtual >0 & Vvirtual< parm.df$Vmax
    Qout <- parm.df$Qoutmax
    
    return(Qout)
}

#' Overflow from retention structure
#' @param Qin is a SpatialPointsDataFrame inflow to structure in step i
#' @param Qout is a SpatialPointsDataFrame regulated outflow from structure in step i
#' @param Vvirtual is the virtual volume in step i, including overflow volume
#' @param Vprevious is the volume in step i-1
#' @param param.df is a SpatialPointsDataFrame of parameters of each subbasin, including df (time step), Vmax (maximum allowable volume of structure) and Qoutmax (maximum regulated allowable outflow from structure)
#' @export
Qoverflow_ret_str <- function(Qin,Qout,Vprevious,Vvirtual,parm.df)
{
    #'case 1 Vvirtual<0 Vvirtual==0
    Qover <- 0

    #' case 2 Vvirtual>parm.df$Vmax
    Qover <- Qin-parm.df$Qoutmax-(parm.df$Vmax-Vprevious)/dt
    
    #' case 3 Vvirtual >0 & Vvirtual< parm.df$Vmax
    Qover <- 0
    
    return(Qover)
}

#' @export
inlet_time <- function(L,n,i,S) ### time of concentration upstream of sewers, based on kinematic wave, chow 501, in minutes
{
    L <- L*3.281 ### meter to feet
    i <- i*0.0394 ### mm to inch
    inlettime <- (0.94*(L^0.6)*(n^0.6))/((i^0.4)*(S^0.3))
    return(inlettime) ### in minutes
}

#' @export
IDF <- function(D,T)
{
    require("dplyr")
    IDF <- read.table("./IDF",header=T)
    IDF <- IDF[IDF$T %in% T,]
    Daux <- D
    if(D<5)
    {
        D <- 5
        cat("\n Warning: D is lower than the defined domain for the IDF \n")
    }
    if(D>2880)
    {
        D <- 2880
        cat("\n Warning: D is greater than the defined domain for the IDF \n")
    }
    tmp <- IDF %>% filter(D0 <= D, D <= D1)
    i <- tmp$a*D^tmp$b
    P <- i*Daux/60
    return(P)
}

#' @param parm.df is SpatialPointsDataFrame pointing to the centroids of the catchment and containing C factor, n, initial loss, permanent loss, dt in s and the id of the upstream subbasin
#' @param I is the rainfall intensity in mm/h for time step i
#' @return he is the effective runoff for time step in i
#' @export
loss_model <- function(I,parm.df)
{
    x <- parm.df
    he@data <- ((I*parm.df@data$dt/3600)-parm.df@data$hi)*parm.df@data$c.factor
    he[he<0] <- 0
    return(he)
}

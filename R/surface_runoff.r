#' time of concentration upstream of sewers in minutes, based on the kinematic wave, chow 501
#' @param L length of overland flow, m
#' @param n is the manning roughness coefficient
#' @param i is the rainfall intensity, mm/h
#' @param S average overland slope m/m
#' @export
inlet_time <- function(L,n,i,S) 
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

#' @param state is DataFrame containing C factor, initial loss, dt and intensity is the rainfall intensity in mm/h for time step i
#' @return out is a dataframe with updated column Qin, meaning the inflow to the drainage network which shall be routed with a routing algorith
#' @export
loss_model <- function(state)
{
    out <- state %>% mutate(Qin=ifelse((intensity/3600-hi)<0,0,0.001*area*(intensity/3600-hi)*c.factor))
    return(out)
}

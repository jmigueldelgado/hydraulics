#' @export
dQdy_circular <- function(Ks,slope,A,R,y) ### for alpha between 45 and 135 I say dQdy can be approximated by a rectangular section
{
    dQdy <- Ks*slope^(1/2)*(5/3)*y^(2/3) ### as in page 287 of chow
    return(dQdy)
}

#' @export
kinematic_wave_celerity_constant_B <- function(B,dQdy) ### page 284 chow
### dQdy is the variation of flow with depth given by manning for each particular section. for the kinematic approximation the section should be close to a rectangular section
{
    ck <- (1/B)*dQdy
    return(ck)
}



#' Muskingum's effluent in pipe or catchment reach as in the documentation of citydrain 2
#' @param Qin is a SpatialPointsDataFrame inflows to reach in step i
#' @param V is a SpatialPointsDataFrame of volumes in reach in step i-1
#' @param Qout is a SpatialPointsDataFrame of outflows from reach in step i
#' @param parm.df is SpatialPointsDataFrame pointing to the centroids of the catchment and containing dt, K, X and the id of the upstream subbasin
#' @param i is the time step
#' @export
Q_out_muskingum <- function(Qin,V,parm.df,i)
{
    dt <- parm.df@data$dt
    K <- parm.df@data$K
    X <- parm.df@data$X

    Cx <- (dt/2-K*X)/(dt/2+K*(1-X))
    Cy <- 1/(dt/2+K*(1-X))
    Qout <- Cx*Qin+Cy*V
    return(Qout)
}

#' Stored volume in step i. from the documentation of citydrain 2
#' @param Qin is a SpatialPointsDataFrame inflows to reach in step i
#' @param V is a SpatialPointsDataFrame of volumes in reach in step i-1
#' @param Qout is a SpatialPointsDataFrame of outflows from reach in step i
#' @param parm.df is SpatialPointsDataFrame pointing to the centroids of the catchment and containing dt, K, X and the id of the upstream subbasin
#' @param i is the time step
#' @export
V_muskingum <- function(Qin,Qe,V,parm.df)
{ 
    Vpresent <- (Qin-Qout)*parm.df$dt[1]+V
    return(Vpresent)
}



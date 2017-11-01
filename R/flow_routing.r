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
#' @param state is a data frame of colums: Qin column of inflows to reach in step i; Vprevious is the column of volumes in reach or basin in step i-1; Qout is the column of outflows from reach in step i; dt, K, X are timestep, K and X as in muskingum
#' @export
Q_out_muskingum <- function(state)
{

    out <- mutate(state,Cx=(dt/2-K*X)/(dt/2+K*(1-X)),Cy=1/(dt/2+K*(1-X)),out=Cx*Qin+Cy*Vprevious)

    return(out)
}

#' Stored volume in step i. from the documentation of citydrain 2
#' @param state is a data frame of colums: Qin column of inflows to reach in step i; Vprevious is the column of volumes in reach or basin in step i-1; Qout is the column of outflows from reach in step i; dt, K, X are timestep, K and X as in muskingum
#' @export
V_muskingum <- function(state)
{ 
    out <- mutate(state,V=(Qin-Qout)*dt[1]+Vprevious)
    return(out)
}



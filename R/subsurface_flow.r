#' cumulative infiltration fate *F*
#' @param K hydraulic conductivity of the soil (parameter see chow page 115)
#' @param phi wetting front capillary pressure head (parameter see chow page 115)
#' @param dTheta difference between initial and final moisture contents of the soil
#' @param F_ cumulative infiltration trial value (it must converge until F==F_)
F <- function(K,t,phi,dTheta,F_)
{
return(K*t + phi*dTheta*log(1+F_/(phi*dTheta)))
}

effective_saturation <- function(theta,theta_r,porosity)
{
    se <- (theta-theta_r)/(porosity-theta_r)
    if(se<0) se <- 0
    if(se>1) se <- 1
    return(se)
}
#' change in the wetting front 
dTheta <- function(se,theta_r,porosity)
{
return((1-se)*(porosity-theta_r))
}

#' brooks-correy equation for wetting front capillary pressure head
#' @param phi_b is obtained in lab by draining a soil in stages
#' @param lambda is obtained in lab by draining a soil in stages
#' @param se is the effective saturation given above
phi <- function(se,phi_b,lambda)
{
return(phi_b*se^(-1/lambda))
}

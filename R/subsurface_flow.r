#' cumulative infiltration fate *F*
#' @param K hydraulic conductivity of the soil (parameter see chow page 115)
#' @param phi wetting front capillary pressure head (parameter see chow page 115)
#' @param dTheta difference between initial and final moisture contents of the soil
#' @param F_ cumulative infiltration trial value (it must converge until F==F_)
F <- function(K,t,phi,dTheta,F_)
{
return(K*t + phi*dTheta*log(1+F_/(phi*dTheta)))
}

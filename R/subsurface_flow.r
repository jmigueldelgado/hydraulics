#' potential infiltration rate ft
#' export
ft <- function(K,phi,dTheta,Ft)
{
  return(K*(phi*dTheta/Ft+1))
}

#' cumulative infiltration rate *F*
#' if ft is less than or equal to i_t
#' @import rootSolve
#' @param K hydraulic conductivity of the soil (parameter see chow page 115)
#' @param phi wetting front capillary pressure head (parameter see chow page 115)
#' @param dTheta difference between initial and final moisture contents of the soil
#' @param F0 cumulative infiltration. Obtain from initial conditions or previous iteration
#' @export
case1 <- function(Ft0,ft0,phi,dTheta,K,dt)
{
  fun <- function(Ft) Ft-Ft0-phi*dTheta*log((Ft+phi*dTheta)/(Ft0+phi*dTheta))-K*dt
  Froot <- uniroot(fun,c(0,10*(Ft0+ft0))$root
  return(Froot)
}

#' cumulative infiltration rate *F*
#' if ft is greater than i_t
#' @import rootSolve
#' @export
case2 <- function(Ft0,i_t,dt)
{
  Ft = Ft0+i_t*dt
  if(ft(K,phi,dTheta,Ft)>i_t) return(Ft)
  else case3()

}

case3 <- function()
{
  Fp = (K*phi*dTheta)/(i_t-K)

}

#' cumulative infiltration rate *F*
#' if ft is less than or equal to i_t
#' @import rootSolve


#' effective saturation
#' export
se <- function(theta,theta_r,porosity)
{
    se <- (theta-theta_r)/(porosity-theta_r)
    if(se<0) se <- 0
    if(se>1) se <- 1
    return(se)
}

#' change in the moisture content when the wetting front passes
#' @param se is the effective saturation and can be obtained by function set
#' @param theta_r is the residual moisture content of the soil after it has been thoroughly drained (in this case a soil parameter that can be obtained from a table)
#' @param porosity is the porosity of the soil
dTheta <- function(se,theta_r,porosity)
{
return((1-se)*(porosity-theta_r))
}

#' brooks-correy equation for wetting front capillary pressure head
#' @param phi_b is obtained in lab by draining a soil in stages
#' @param lambda is obtained in lab by draining a soil in stages
#' @param se is the effective saturation given above
#' export
phi <- function(se,phi_b,lambda)
{
return(phi_b*se^(-1/lambda))
}

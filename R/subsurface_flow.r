#' potential infiltration rate ft
#' export
calc_ft <- function(K,phi,dTheta,Ft)
{
  return(K*(phi*dTheta/Ft+1))
}


calc_Ft <- function(Ft0,ft0,phi,dTheta,K,dt,i_t)
{
  ft=calc_ft(K,phi,dTheta,Ft0)

  if(ft<=i_t) {
    Ft = case1(Ft0,ft0,phi,dTheta,K,dt)
  } else {
    if(calc_ft(K,phi,dTheta,Ft0+i_t*dt)>i_t) {
      Ft = case2(Ft0,dt,i_t)
    } else {
      Ft=case3(Ft0,ft0,phi,dTheta,K,dt,i_t)
    }
  }

  return(Ft)
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
  Froot <- uniroot(fun,c(0,10*(Ft0+ft0)))$root
  return(Froot)
}

#' cumulative infiltration rate *F*
#' if ft is greater than i_t
#' @export
case2 <- function(Ft0,dt,i_t)
{
  Ft = Ft0+i_t*dt
  return(Ft)
}

#' cumulative infiltration rate *F*
#' if ft is less than or equal to i_t
#' @import rootSolve
#' @param K hydraulic conductivity of the soil (parameter see chow page 115)
#' @param phi wetting front capillary pressure head (parameter see chow page 115)
#' @param dTheta difference between initial and final moisture contents of the soil
#' @param F0 cumulative infiltration. Obtain from initial conditions or previous iteration
#' @export
case3 <- function(Ft0,ft0,phi,dTheta,K,dt,i_t)
{
  Fp = (K*phi*dTheta)/(i_t-K)
  ponding_time = (Fp-Ft0)/i_t
  fun <- function(Ft) Ft-Fp-phi*dTheta*log((Ft+phi*dTheta)/(Fp+phi*dTheta))-K*(dt-ponding_time)
  if(is.infinite(ft0)) ft0 <- 999999
  Froot=uniroot(fun,c(0,10*(Fp+ft0)))$root

  return(Froot)
}



#' effective saturation
#' @export
calc_se <- function(theta,theta_r,porosity)
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
calc_dTheta <- function(se,theta_r,porosity)
{
return((1-se)*(porosity-theta_r))
}

#' brooks-correy equation for wetting front capillary pressure head
#' @param phi_b is obtained in lab by draining a soil in stages
#' @param lambda is obtained in lab by draining a soil in stages
#' @param se is the effective saturation given above
#' export
calc_phi <- function(se,phi_b,lambda)
{
return(phi_b*se^(-1/lambda))
}

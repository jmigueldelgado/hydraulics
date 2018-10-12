library(rootSolve)
library(hydraulics)

i_t <- 20 # mm/h
K <- 6.5 # mm/h
phi <- 166.8 # mm
theta_r <- 0.02

dt <- 10/60
porosity <- 0.501
theta_i <- 0.3
Ft <- 0

#se <- calc_se(theta,theta_r,porosity)
#phi <- calc_phi(se,phi_b,porosity)
dTheta <- porosity-theta_i #calc_dTheta(se,theta_r,porosity) ??

Ft0=Ft
ft=calc_ft(K,phi,dTheta,Ft0)

calc_Ft(Ft0,phi,dTheta,K,dt,i_t)

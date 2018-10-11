library(rootSolve)

lambda <- 3
i_t <- 20
K <- 0.65
dt=60
theta_r <- 0.02
porosity <- 0.501
theta <- 0


se <- calc_se(theta,theta_r,porosity)
phi <- calc_phi(se,phi_b,porosity)
theta <- calc_dTheta(se,theta_r,porosity)

Ft0=Ft
ft=calc_ft(K,phi,dTheta,Ft0)
ft0 <- ft

if(ft<=i_t) {
    Ft = case1(Ft0,ft0,phi,dTheta,K,dt)
  } else {
    if(calc_ft(K,phi,dTheta,Ft0+i_t*dt)>i_t) {
      Ft = case2(Ft0,dt,i_t)
    } else {
        Ft=case3(Ft0,ft0,phi,dTheta,K,dt,i_t)
    }
}


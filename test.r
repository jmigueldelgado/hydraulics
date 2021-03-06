library(rootSolve)
library(hydraulics)

i_t <- 20 # mm/h
K <- 6.5 # mm/h
phi <- 166.8 # mm
theta_r <- 0.02
soil_depth <- 2000 #mm


runoff=0
tt <- 0
dt <- 10/60
porosity <- 0.501
theta_i <- 0.3
Ft <- 0
Li <- 0

#se <- calc_se(theta,theta_r,porosity)
#phi <- calc_phi(se,phi_b,porosity)
dTheta <- porosity-theta_i #calc_dTheta(se,theta_r,porosity) ??

while(Li<soil_depth)
{
    
    tt <- tt+dt
    Ft0=Ft
    ft=calc_ft(K,phi,dTheta,Ft0)

    Ft=calc_Ft(Ft0,phi,dTheta,K,dt,i_t)
    Li <- Ft/dTheta

    if( (i_t-ft)>0)
    {
        runoff=runoff+(i_t-ft)*dt
    }

}



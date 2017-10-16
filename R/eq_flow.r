
#' @export
rectangular_flow_area <- function(h,b) ## h is flow height, b is width of the rectangular section
{
    A <- b*h
    return(A)
}

#' @param h is flow height
#' @param a1 is height of first section
#' @param b is width of the rectangular section
#' @export
double_rectangular_flow_area <- function(h,a1,b)
{
    if(h>a1)
    {
        A <- rectangular_flow_area(a1,b) + rectangular_flow_area(h-a1,b)
    } else
    {
        A <- rectangular_flow_area(h,b)
    }
    return(A)
}

#' @param h is flow height
#' @param b_base is width of the base of the trapezoid
#' @param b is width of the upper side of the trapezoid
#' @param a is the height of the trapezoid
#' @export
trapezoidal_flow_area <- function(h,b_base,b,a)
{
    A <- b_base*h*(b-b_base)/a
    return(A)
}

#' @param h is flow height
#' @param b_base is width of the base of the trapezoid
#' @param b is a vector of widths of the upper side of the trapezoids in ascending order
#' @param a is a vector of the heights of the trapezoid in ascending order
#' @export
n_trapezoids_flow_area <- function(h,b_base,b,a)

#' @export
circular_flow_area <- function(alpha,D) ## D is diameter of section, alpha is angle of wetted perimeter
{
    A <- ((D^2)/4)*(alpha-sin(2*alpha)/2)
    return(A)
}

#' @export
circular_wetted_angle <- function(h,D) # h is height and  D is diameter
{
    alpha <- acos(1-h/(D/2))
    return(alpha)
}

#' @export
circular_depth <- function(alpha,D)
{
    y <- D/2*(1-cos(alpha))
    return(y)
}

#' @export
circular_hydraulic_radius <- function(D,alpha) ## D is diameter, alpha is wetted angle
{
    R <- (D/4)*(1-sin(2*alpha)/(2*alpha))
    return(R)
}

#' @export
mannings_eq_flow <- function(Ks,slope,A,R) ## Ks is strickler coefficient, slope is slope of the energy curve, A is section area, R is hydraulic radius
{
    Q <- A*Ks*R^(2/3)*slope^(1/2)
    return(Q)
}


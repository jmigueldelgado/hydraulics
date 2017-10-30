#' Virtual volume in retention structure
#' @param ret_str_state is a dataframe with columns:
#' @param Qin is a column with inflow to structure in step i
#' @param Qoverflow is a column unregulated overflow from structure in step i
#' @param Vvirtual is the virtual volume in step i, including overflow volume
#' @param Vprevious is the volume in step i-1
#' @param dt is a constant column (time step)
#' @param Vmax is a constant column (maximum allowable volume of structure)
#' @param Qoutmax is a constant column (maximum regulated allowable outflow from structure)
#' @export
Virtual_retention <- function(ret_str_state)
{
    out <- ret_str_state %>%
        mutate(Vvirtual=(Qin-Qoutmax)*dt + Vprevious)
    return(out)
}

#' Actual volume in retention structure
#' @param ret_str_state is a dataframe with columns:
#' @param Qin is a column with inflow to structure in step i
#' @param Qoverflow is a column unregulated overflow from structure in step i
#' @param Vvirtual is the virtual volume in step i, including overflow volume
#' @param Vprevious is the volume in step i-1
#' @param dt is a constant column (time step)
#' @param Vmax is a constant column (maximum allowable volume of structure)
#' @param Qoutmax is a constant column (maximum regulated allowable outflow from structure)
#' @export
Actual_retention <- function(ret_str_state)
{
    out <- ret_str_state %>%
        mutate(V=ifelse(Vvirtual<=0, 0,V)) %>%
        mutate(V=ifelse(Vvirtual > 0, (Qin-Qoutmax)*dt+Vprevious,V)) %>%
        mutate(V=ifelse(Vvirtual>Vmax, Vmax,V))

    return(out)
}

#' Outflow from retention structure
#' @param ret_str_state is a dataframe with columns:
#' @param Qin is a column with inflow to structure in step i
#' @param Qoverflow is a column unregulated overflow from structure in step i
#' @param Vvirtual is the virtual volume in step i, including overflow volume
#' @param Vprevious is the volume in step i-1
#' @param dt is a constant column (time step)
#' @param Vmax is a constant column (maximum allowable volume of structure)
#' @param Qoutmax is a constant column (maximum regulated allowable outflow from structure)
#' @export
Qoutflow_ret_str <- function(ret_str_state)
{

    out <- ret_str_state %>%
        mutate(Qout=ifelse(Vvirtual<=0, Vprevious/dt + Qin,Qout)) %>%
        mutate(Qout=ifelse(Vvirtual > 0, Qoutmax,Qout)) %>%
        mutate(Qout=ifelse(Vvirtual>Vmax, Qoutmax,Qout))
    
    return(out)
}

#' Overflow from retention structure
#' @param ret_str_state is a dataframe with columns:
#' @param Qin is a column with inflow to structure in step i
#' @param Qoverflow is a column unregulated overflow from structure in step i
#' @param Vvirtual is the virtual volume in step i, including overflow volume
#' @param Vprevious is the volume in step i-1
#' @param dt is a constant column (time step)
#' @param Vmax is a constant column (maximum allowable volume of structure)
#' @param Qoutmax is a constant column (maximum regulated allowable outflow from structure)
#' @export
Qoverflow_ret_str <- function(ret_str_state)
{
    out <- ret_str_state %>%
        mutate(Qoverflow=ifelse(Vvirtual<=0, 0,Qoverflow)) %>%
        mutate(Qoverflow=ifelse(Vvirtual > 0, 0,Qoverflow)) %>%
        mutate(Qoverflow=ifelse(Vvirtual>Vmax, Qin-Qoutmax-(Vmax-Vprevious)/dt,Qoverflow))
    
    return(out)
}

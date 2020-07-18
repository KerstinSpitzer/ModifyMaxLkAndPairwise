modify_pairwise <- function(rhohat,theta,n,l,all.ests=TRUE)
{
  #recombination rate in 1/bp:
  rhohatperbp <- rhohat/(l-1)

  #modifying constant:
  cm <-  0.09901367*n  + 5.778474e-05*l + 52.99201*theta + 8.964817*(1/n) - 1.845533e-09*(l^2) - 2038.776*(theta^2) + 0.005957148*(l*theta) - 0.0003339842*(n*l*theta) + 42.33759*rhohatperbp - 0.0004998093*(1/rhohatperbp) - 2.614086*(rhohatperbp*n) + 0.000869142*(rhohatperbp*l) + 623.8837*(rhohatperbp*theta) - 1.84556

  #modified estimate:
  if(all.ests==TRUE)
  {
    resultperbp <- rhohatperbp * cm
  }
  else
  {
    if(rhohatperbp>=0.002 && rhohatperbp<=0.03 && !(is.na(rhohatperbp)))
    {
      resultperbp <- rhohatperbp * cm
    }
    else
    {
      resultperbp <- rhohatperbp
    }
  }

  result <- resultperbp*(l-1)

  return(result)
}

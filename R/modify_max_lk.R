modify_max_lk <- function(rhohat,theta,n,l,all.ests=TRUE)
{
  #constant term in bias:
  ConstBias <- 0.0002934496*n + 0.0000000366*l - 0.000006311*(n^2) +  0.02106979*(1/n) - 0.000002503*(l*theta) - 0.0048581

  #modifying constant:
  cm <- 15.41996*theta + 0.108176*n - 0.001844253*(n^2) + 8.516768*(1/n) + 0.0000321*l - 615.2638*(1/l) - 0.000001411*(n*l) - 0.2714421*(n*theta) - 0.0003741073*(l*theta) - 0.8115302

  #modified estimate:
  if(all.ests==TRUE)
  {
    result <- (rhohat - ConstBias) * cm
  }
  else
  {
    if(rhohat>=0.002 && rhohat<=0.03 && !(is.na(rhohat)))
    {
      result <- (rhohat - ConstBias) * cm
    }
    else
    {
      result <- rhohat
    }
  }

  return(result)
}




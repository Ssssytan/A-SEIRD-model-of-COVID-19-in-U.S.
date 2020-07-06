# ------------------------------------------------------------------------------
# ---------------------------------RSS function---------------------------------
# ------------------------------------------------------------------------------

rss = function(OPARS, death, N, S, E = 1L, I = 0, R = 0, D = 0)
{
  R0 = OPARS[1]
  alpha = OPARS[2]
  
  pars.seird = c(beta = R0 * gamma0,
                 gamma = gamma0,
                 sigma = sigma,
                 alpha = alpha,
                 N = N)
  init.serid = c(S = N - E - I - R - D,
                 E = E,
                 I = I,
                 R = R,
                 D = D)
  times = 1:length(death) - 1
  death.hat = SEIRD(pars = pars.seird,init = init.serid, time = times)$results$D
  return(sum((as.integer(death.hat) - death)^2))
}

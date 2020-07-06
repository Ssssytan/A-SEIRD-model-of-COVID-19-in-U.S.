library(EpiDynamics)
library(ggplot2)
library(reshape2)

library(deSolve)

SEIRD = function (pars = NULL, init = NULL, time = NULL) 
{
  if (is.null(pars)) {stop("undefined 'pars'")}
  if (is.null(init)) {stop("undefined 'inits'")}
  if (is.null(time)) {stop("undefined 'time'")}
  
  function1 = function(pars = NULL, init = NULL, time = NULL)
  {
    function2 = function(time, init, pars)
    {
      with(as.list(c(init, pars)), {
        dS = -beta * S * I / N
        dE =  beta * S * I / N - sigma * E
        dI = sigma * E - gamma * I
        dR = (1 - alpha)  * gamma * I
        dD = alpha * gamma * I
        list(c(dS, dE, dI, dR, dD))
      })
    }
    init = c(init["S"], init["E"], init["I"], init["R"], init["D"])
    output = ode(times = time, func = function2, y = init, parms = pars)
    return(output)
  }
  output = function1(pars = pars, init = init, time = time)
  return(list(model = function1,
              pars = pars,
              init = init, 
              time = time,
              results = as.data.frame(output)))
}
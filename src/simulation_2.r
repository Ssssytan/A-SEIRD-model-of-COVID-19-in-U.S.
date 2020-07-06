source('src/DataCollecter.r')
source('src/SEIRD_model.r')
source('src/rss.r')


# Hyperparameters
gamma0 = 1/10
sigma = 1/4

# Limitaion for R0 and alpha
par.lower = c(1, 0.)
par.upper = c(15, 1)

# Country
E = 1L
I = 0L
N = sum(population$Population)

# divide stages
div = 75L
deaths = list(stage1 = deaths.country[1:div],
                      stage2 = deaths.country[(1+div):length(deaths.country)])

par.hat = optim(par = c(5, 0.005),fn = rss, method = 'L-BFGS-B',
                        lower = par.lower, upper = par.upper,
                        death = deaths$stage1,
                        N = N, E = E, I = I)$par

round(par.hat, 3)

SEIRD.hat = SEIRD(pars =  c(beta = par.hat[1] * gamma0,
                                  gamma = gamma0,
                                  sigma = sigma,
                                  alpha = par.hat[2],
                                  N = N),
                        init = c(S = N - E - I,
                                 E = E,
                                 I = I,
                                 R = 0,
                                 D = 0), 
                        time = 1:div - 1)$results$D

SEIRD.hat = as.integer(SEIRD.hat)

cmp.tb = data.frame(Esitimate = SEIRD.hat , Real = deaths$stage1)
matplot(cmp.tb, type=c('l', 'l'), lwd=2, col=c('red','black'), xlab = '', ylab='Death Number')
legend(1, max(cmp.tb), colnames(cmp.tb), col=c('red','black'), lty=1,lwd=2,cex=1)
grid();  title('Death in U.S. Before Social Distancing (Since 2020-01-22)')

rm(list = ls())

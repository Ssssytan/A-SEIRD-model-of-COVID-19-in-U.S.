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
div = 70L
deaths = list(stage1 = deaths.country[1:div],
              stage2 = deaths.country[(1+div):length(deaths.country)])

par.hat = optim(par = c(5, 0.005),fn = rss, method = 'L-BFGS-B',
                lower = par.lower, upper = par.upper,
                death = deaths$stage1,
                N = N, E = E, I = I)$par

round(par.hat, 3)

SEIRD.1 = SEIRD(pars =  c(beta = par.hat[1] * gamma0,
                          gamma = gamma0,
                          sigma = sigma,
                          alpha = par.hat[2],
                          N = N),
                  init = c(S = N - E - I,
                           E = E,
                           I = I,
                           R = 0,
                           D = 0), 
                  time = 1:div)

init = SEIRD.1$results[div,]
death.hat.1 = SEIRD.1$results$D
rm(SEIRD.1)


death.hat.2 = SEIRD(pars =  c(beta = par.hat[1] * gamma0 * 0.145,
                              gamma = gamma0,
                              sigma = sigma,
                              alpha = par.hat[2],
                              N = N),
                    init =  c(S = init$S,
                              E = init$E,
                              I = init$I,
                              R = init$R,
                              D = init$D), 
                    time = 1:length(deaths$stage2))$results$D

death.hat.2 = as.integer(death.hat.2)


death.cmp = data.frame(Esitimate = death.hat.2 , Real = deaths$stage2)
matplot(death.cmp, type=c('l','l'), lwd=2, col=c('red','black'), xlab = 'Time', ylab='Number')
legend(1, max(death.cmp), colnames(death.cmp), col=c('red','black'), lty=1,lwd=2,cex=1)
grid();  title('Death in U.S. After Social Distancing (Since 2020-04-06)')


death.cmp = data.frame(Esitimate = append(death.hat.1, death.hat.2) ,
                       Real = deaths.country)
matplot(death.cmp, type=c('l','l'), lwd=2, col=c('red','black'), xlab = 'Time', ylab='Number')
legend(1, max(death.cmp), colnames(death.cmp), col=c('red','black'), lty=1,lwd=2,cex=1)
grid();  title('Death in U.S. Till now')

# rm(list=ls())


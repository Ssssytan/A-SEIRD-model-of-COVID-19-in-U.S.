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
deaths = deaths.country
par.hat = optim(par = c(5, 0.005),fn = rss, method = 'L-BFGS-B',
                        lower = par.lower, upper = par.upper,
                        death = deaths,
                        N = N, E = E, I = I)$par

par.hat

rm(list = ls())

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

SEIRD.2 = SEIRD(pars =  c(beta = par.hat[1] * gamma0 * 0.145,
                          gamma = gamma0,
                          sigma = sigma,
                          alpha = par.hat[2],
                          N = N),
                init =  c(S = init$S,
                          E = init$E,
                          I = init$I,
                          R = init$R,
                          D = init$D), 
                time = 1:length(deaths$stage2))

SEIRD.all = SEIRD.1
SEIRD.all$results = rbind(SEIRD.1$results, SEIRD.2$results)
SEIRD.all$results$time = 1:length(SEIRD.all$results$time)
PlotMods(SEIRD.all)

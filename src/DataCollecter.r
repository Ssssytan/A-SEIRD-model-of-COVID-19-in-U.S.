# --------------------------------------------------------------------------
# --------------------------------Load Data---------------------------------
# --------------------------------------------------------------------------

deaths.state = read.csv('dataset/us-states.csv')
cbind(colnames(deaths.state), 1:length(colnames(deaths.state)))
deaths.state = deaths.state[,-c(3:4)]
deaths.state$date = as.Date(deaths.state$date)
deaths.state$state = as.character(deaths.state$state)

get_state_death = function(state){
  raw = deaths.state[deaths.state$state == state,]
  death = raw$death
  names(death) = raw$date
  death
}

deaths.country = read.csv('dataset/time_series_covid19_deaths_US.csv')
population = deaths.country[c('Province_State', 'Population')]
population = aggregate(Population~ Province_State, data = population,FUN = sum)

deaths.country = deaths.country[13:length(deaths.country)]
deaths.country = colSums(deaths.country)
names(deaths.country) = 1:length(deaths.country) +  as.Date('2020/1/22') - 1

# tmp.tb = data.frame(NewYork = get_state_death('New York')[1:80],
#                     NewJersey = get_state_death('New Jersey')[1:80],
#                     Massachusetts = get_state_death('Massachusetts')[1:80]	)
# matplot(tmp.tb, type='l', lwd=2, col=c('red','black','blue'), xlab = 'Day', ylab='Death Number')
# legend(1, max(tmp.tb), colnames(tmp.tb), col=c('red','black','blue'), lty=1,lwd=2,cex=1)
# grid();  title('Death in 3 states of U.S. (Since 2020-03-01)')

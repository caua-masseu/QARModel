library(quantreg)

#for n = 100
rep = 1000

n = 100
ut <- rnorm(n)

TAU <- seq(0.1, 0.9, by = 0.1)
alpha = 0.95


for(i in 1:rep){
  yt_95_n = arima.sim(n = n, model = list(ar = c(0.95)), n.start = 100)
  yt_9_n = arima.sim(n = n, model = list(ar = c(0.9)), n.start = 100)
  yt_6_n = arima.sim(n = n, model = list(ar = c(0.6)), n.start = 100)
  
  yt_95_t = arima.sim(n = n, model = list(ar = c(0.95)), n.start = 100, rand.gen = function(n) rt(n, df = 3))
  yt_9_t = arima.sim(n = n, model = list(ar = c(0.9)), n.start = 100, rand.gen = function(n) rt(n, df = 3) )
  yt_6_t = arima.sim(n = n, model = list(ar = c(0.6)), n.start = 100, rand.gen = function(n) rt(n, df = 3))
  
  
  fit_95_n <- rq(formula = yt_95_n[2:n] ~ yt_95_n[1:(n-1)], tau = TAU)
}

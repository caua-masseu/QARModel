library(quantreg)
library(Qtools)

#for n = 100
rep = 1000

n = 100

TAU <- seq(0.1, 0.9, by = 0.01)

reject_95_n_hHS3 <- reject_9_n_hHS3 <- reject_6_n_hHS3 <- reject_95_n_hHS <- reject_9_n_hHS <- reject_6_n_hHS <- 0
reject_95_n_hB <- reject_9_n_hB <- reject_6_n_hB <- reject_95_n_hB06 <- reject_9_n_hB06 <- reject_6_n_hB06 <- 0
alpha = 0.05
critical_value <- 1.923
  
hHS = mean(bandwidth.rq(p = TAU, n = n, alpha = alpha, hs = TRUE))
hHS_3 = mean(3*hHS)
hB = mean(bandwidth.rq(p = TAU, n = n, alpha = alpha, hs = FALSE))
hB_06 = mean(0.6*hB)

for(i in 1:rep){
  set.seed(i)
  yt_95_n = arima.sim(n = n, model = list(ar = c(0.95)), n.start = 100)
  yt_9_n = arima.sim(n = n, model = list(ar = c(0.9)), n.start = 100)
  yt_6_n = arima.sim(n = n, model = list(ar = c(0.6)), n.start = 100)
  
  yt_95_t = arima.sim(n = n, model = list(ar = c(0.95)), n.start = 100, rand.gen = function(n) rt(n, df = 3))
  yt_9_t = arima.sim(n = n, model = list(ar = c(0.9)), n.start = 100, rand.gen = function(n) rt(n, df = 3) )
  yt_6_t = arima.sim(n = n, model = list(ar = c(0.6)), n.start = 100, rand.gen = function(n) rt(n, df = 3))
  

  
  tr_95_n_hHS3 <- KhmaladzeTest(yt_95_n[2:n] ~ yt_95_n[1:(n-1)], taus = TAU, nullH = "location")
  tr_9_n_hHS3 <- KhmaladzeTest(yt_9_n[2:n] ~ yt_9_n[1:(n-1)], taus = TAU, nullH = "location")
  tr_6_n_hHS3 <- KhmaladzeTest(yt_6_n[2:n] ~ yt_6_n[1:(n-1)], taus = TAU, nullH = "location")
  
  tr_95_n_hHS <- KhmaladzeTest(yt_95_n[2:n] ~ yt_95_n[1:(n-1)], taus = TAU, nullH = "location")
  tr_9_n_hHS <- KhmaladzeTest(yt_9_n[2:n] ~ yt_9_n[1:(n-1)], taus = TAU, nullH = "location")
  tr_6_n_hHS <- KhmaladzeTest(yt_6_n[2:n] ~ yt_6_n[1:(n-1)], taus = TAU, nullH = "location")
  
  tr_95_n_hB <- KhmaladzeTest(yt_95_n[2:n] ~ yt_95_n[1:(n-1)], taus = TAU, nullH = "location")
  tr_9_n_hB <- KhmaladzeTest(yt_9_n[2:n] ~ yt_9_n[1:(n-1)], taus = TAU, nullH = "location")
  tr_6_n_hB <- KhmaladzeTest(yt_6_n[2:n] ~ yt_6_n[1:(n-1)], taus = TAU, nullH = "location")
  
  tr_95_n_hB06 <- KhmaladzeTest(yt_95_n[2:n] ~ yt_95_n[1:(n-1)], taus = TAU, nullH = "location")
  tr_9_n_hB06 <- KhmaladzeTest(yt_9_n[2:n] ~ yt_9_n[1:(n-1)], taus = TAU, nullH = "location")
  tr_6_n_hB06 <- KhmaladzeTest(yt_6_n[2:n] ~ yt_6_n[1:(n-1)], taus = TAU, nullH = "location")
  
  if(tr_95_n_hHS3$Tn > critical_value){
    reject_95_n_hHS3 <- reject_95_n_hHS3 + 1
  }
  if(tr_9_n_hHS3$Tn > critical_value){
    reject_9_n_hHS3 <- reject_9_n_hHS3 + 1
  }
  if(tr_6_n_hHS3$Tn > critical_value){
    reject_6_n_hHS3 <- reject_6_n_hHS3 + 1
  }
  if(tr_95_n_hHS$Tn > critical_value){
    reject_95_n_hHS <- reject_95_n_hHS + 1
  }
  if(tr_9_n_hHS$Tn > critical_value){
    reject_9_n_hHS <- reject_9_n_hHS + 1
  }
  if(tr_6_n_hHS$Tn > critical_value){
    reject_6_n_hHS <- reject_6_n_hHS + 1
  }
  if(tr_95_n_hB$Tn > critical_value){
    reject_95_n_hB <- reject_95_n_hB + 1
  }
  if(tr_9_n_hB$Tn > critical_value){
    reject_9_n_hB <- reject_9_n_hB + 1
  }
  if(tr_6_n_hB$Tn > critical_value){
    reject_6_n_hB <- reject_6_n_hB + 1
  }
  if(tr_95_n_hB06$Tn > critical_value){
    reject_95_n_hB06 <- reject_95_n_hB06 + 1
  }
  if(tr_9_n_hB06$Tn > critical_value){
    reject_9_n_hB06 <- reject_9_n_hB06 + 1
  }
  if(tr_6_n_hB06$Tn > critical_value){
    reject_6_n_hB06 <- reject_6_n_hB06 + 1
  }
}

# fit_95_n <- rq(formula = yt_95_n[2:n] ~ yt_95_n[1:(n-1)], tau = TAU)
# fit_9_n <- rq(formula = yt_9_n[2:n] ~ yt_9_n[1:(n-1)], tau = TAU)
# fit_6_n <- rq(formula = yt_6_n[2:n] ~ yt_6_n[1:(n-1)], tau = TAU)
# 
# fit_95_t <- rq(formula = yt_95_t[2:n] ~ yt_95_t[1:(n-1)], tau = TAU)
# fit_9_t <- rq(formula = yt_9_t[2:n] ~ yt_9_t[1:(n-1)], tau = TAU)
# fit_6_t <- rq(formula = yt_6_t[2:n] ~ yt_6_t[1:(n-1)], tau = TAU)
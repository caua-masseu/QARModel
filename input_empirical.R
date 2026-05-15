library(quantreg)

gprice <- read.csv("gasprice.csv")[1:699,]

#Table 6
fit_gprice <- rq(formula = gprice[5:699,2] ~ gprice[4:698,2] + gprice[3:697,2] + gprice[2:696,2] + gprice[1:695,2], tau = TAU)
print(apply(coef(fit_gprice)[2:5, ], 2, sum))

tr_gas <- KhmaladzeTest(gprice[5:699,2] ~ gprice[4:698,2] + gprice[3:697,2] + gprice[2:696,2] + gprice[1:695,2], nullH = "location", taus = TAU)

tr_gas$Tn
  
unemp <- read.csv("UNRATE.csv")[1:672,]
unemp_q <- numeric(224)

for(i in 1:224){
  unemp_q[i] <- (unemp[3*i-2,2] + unemp[3*i-1,2] + unemp[3*i,2])/3
}

fit_unemp_q <- rq(formula = unemp_q[3:224] ~ unemp_q[2:223] + unemp_q[1:222], tau = TAU)
fit_ols_unemp_q <- arima(unemp_q, order = c(2,0,0), method = "CSS")
print(apply(coef(fit_unemp_q)[2:3, ], 2, sum))

print(sum(coef(fit_ols_unemp_q)[1:2]))

unempl <- openintro::unempl
unempl <- unempl[1:107,2, drop = TRUE]
fit_unemp_a <- rq(formula = unempl[4:107] ~ unemp_q[3:106] + unemp_q[2:105] + unempl[1:104], tau = TAU)

print(apply(coef(fit_unemp_a)[2:4, ], 2, sum))

TAU = seq(0.5, 0.95, by = 0.05)
hHS_3 = 3*bandwidth.rq(p = TAU, n = n, alpha = alpha, hs = TRUE)

tr_unemp_q <- KhmaladzeTest(unemp_q[3:224] ~ unemp_q[2:223] + unemp_q[1:222], nullH = "location", taus = TAU, h = hHS_3)


# 1. Prepare ADF-style data (4 lags)
y <- gprice[,2] 
n_total <- length(y)
dy <- diff(y)

# Target variable (y_t)
y_target <- y[5:n_total]

# Regressors: Intercept, Level Lag (y_{t-1}), and 3 Difference Lags
y_lag1 <- y[4:(n_total - 1)]
dy_lag1 <- dy[3:(n_total - 2)]
dy_lag2 <- dy[2:(n_total - 3)]
dy_lag3 <- dy[1:(n_total - 4)]

# 2. Define fine grid and effective sample size
TAU_GRID <- seq(0.1, 0.9, by = 0.01) # Grid over T = [0.1, 0.9]
n_eff <- length(y_target)            # n = 695

# 3. Fit the model and extract the level lag coefficient (delta_0)
# Formula: y_t = intercept + delta_0 * y_{t-1} + delta_1 * dy_{t-1} ...
fit <- rq(y_target ~ y_lag1 + dy_lag1 + dy_lag2 + dy_lag3, tau = TAU_GRID)

# The root delta_0(tau) is the coefficient of y_lag1 (index 2)
delta_0_tau <- coef(fit)[7]

# 4. Compute the empirical process Un(tau)
Un_tau <- n_eff * (delta_0_tau - 1)

# 5. Calculate Test Statistics
# QKS is the supremum of the absolute process [4]
QKS_stat <- max(abs(Un_tau))

# QCM is the integral of Un^2 over d_tau [4]
# Range is [0.1, 0.9], so width is 0.8
QCM_stat <- mean(Un_tau^2) * 0.8

cat("Fixed QKS Statistic:", round(QKS_stat, 2), "(Target: 35.79)\n")
cat("Fixed QCM Statistic:", round(QCM_stat, 2), "(Target: 320.41)\n")



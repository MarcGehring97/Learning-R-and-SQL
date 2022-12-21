#####################################################
# Solutions to HW2: Applied Financial Econometrics  #
# March 10, 2022                 
# Martin Waibel
#####################################################

rm(list = ls()) # Clear variables
# Set the working directory
pwd = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(pwd)

###############
# Question 1
###############
library(haven)
library(stargazer)
library(lmtest)
library(AER)
library(urca)

# Load the dataset
pension <- read_dta('data/pension.dta')
attach(pension)

#(a) OLS regression
fit_ols <- lm(pira ~ p401k + inc + incsq + age + agesq)
summary(fit_ols)

#(b) -> See PDF solutions

#(c) -> See PDF solutions

#(d) Reduced Form Regression with white standard error
reduced_form <- lm(p401k ~ e401k + inc + incsq + age + agesq) # partial correction: x ~ IV_x
white_std <- coeftest(reduced_form, vcov = vcovHC(reduced_form, type = "HC0")) # white standard errors

#(e) IV regression
# Method 1: 2step approach
# First stage
IV_1step <- lm(p401k ~ e401k + inc + incsq + age + agesq) # reduced form regression: y ~ IV var + exo regressors
hatp401k <- fitted(IV_1step)

# Second stage
IV_2step <- lm(pira ~ hatp401k + inc + incsq + age + agesq) # structural form: y ~ fitted endo var + exo regressors
stargazer(IV_2step)

# Alternative: Use pre-programmed ivreg() to do two steps directly
# The ivreg function from the AER package will give you exactly the same betas  like the two-tep approach above.
fit_iv<- ivreg(pira ~  p401k + inc + incsq + age + agesq | e401k + inc + incsq + age + agesq)

# Get the confidence Interval of IV estimates
confint(fit_iv)

# Test if there is 1. A Weak Instrument issue, null hypo is WEAK. Test result is REJECT
#                  2. Wu-Hausman test, null hypo is both OLS and IV are consistent. 
#                     Test result is REJECT. 
summary(fit_iv, vcov = sandwich, df = Inf, diagnostics = TRUE)

# Export the regression tables
stargazer(fit_ols, reduced_form, IV_2step, se=list(NULL, white_std[,2], NULL),
          column.labels=c("default","robust","default"), align=TRUE,
          title = "Regression output Exercise 1",
          out = paste(pwd,"/","output/tables/reg_out_ex_1.tex", sep=""))
detach(pension)



###############
# Question 2
###############
library(xtable)

# Read in the dataset
loans <- read_dta('data/loans.dta')
attach(loans)

# (a) Probit model
probitmodel <- glm(approve ~ white, family = binomial(link = "probit"))
summary(probitmodel) 

# The estimated probability is G(X*beta) where G is the pdf of a normal distribution
# Get the fitted values for non-whites:
y_nonwhites         <- probitmodel$coefficients[[1]] + probitmodel$coefficients[[2]]*0
esti_prob_nonwhites <- pnorm(y_nonwhites)

# Get the fitted values for whites:
y_whites            <- probitmodel$coefficients[[1]] + probitmodel$coefficients[[2]]*1
esti_prob_whites    <- pnorm(y_whites)

# This result could also be retrieved directly from the probitmodel result
unique(probitmodel$fitted.values)


# (b) Linear model
ols_loans  <- lm(approve ~ white)

#stargazer(probitmodel, ols_loans, 
#          caption = 'approve~white', label = 'probitols',
#          out = paste(pwd,"/","output/tables/reg_out_ex_2_a_b.tex", sep=""))

# Bind regression results in a R table
esti_prob <- rbind(cbind(esti_prob_nonwhites, esti_prob_whites), 
                   cbind(ols_loans$coefficients[[1]],
                         ols_loans$coefficients[[1]] + ols_loans$coefficients[[2]]))
colnames(esti_prob) <- c('nonwhites', 'whites')
rownames(esti_prob) <- c('Probit', 'OLS')
xtable(esti_prob, caption = 'Estimated Loan Approval Probability')
# Export to Latex
print(xtable(esti_prob, type = "latex", caption = 'Estimated Loan Approval Probability'), 
      file = paste(pwd,"/","output/tables/reg_out_ex_2_a_b_2.tex", sep=""))


# (c) Add additional covariates to the Probit model
probitnew <- glm(approve ~ white + hrat + obrat + loanprc + unem + male + married 
                 + dep + sch + cosign + chist + pubrec + mortlat1 + mortlat2 + vr,
                 family = binomial(link = "probit"))
summary(probitnew) 

# (d) Estimate the Logit model with the additional covariates
logitnew  <- glm(approve ~ white + hrat + obrat + loanprc + unem + male + married 
                 + dep + sch + cosign + chist + pubrec + mortlat1 + mortlat2 + vr,
                 family = binomial(link = "logit"))
stargazer(probitnew, logitnew, 
          single.row = TRUE, 
          column.sep.width = "1pt",
          caption = 'Extended Regression',
          label = 'probitlogit',
          title = "Regression output Probit/Logit with additional covariates"
          #,out = paste(pwd,"/","output/tables/reg_out_ex_2_c_d.tex", sep="")
          )


## Importantly, the above regression results do NOT directly give the partial effects.
## To get the partial effects on needs to evaluate the obtained coefficients estimates
## according to the respective functional form assumption.
# Compute the scale factors 
beta.logit  <- coef(logitnew)
beta.probit <- coef(probitnew)

rep.col<-function(x,n){
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}

scalefactor <- function(X, beta, method){
  avg           <- apply(X, MARGIN = 2, mean, na.rm = TRUE) # skip missing variables
  xibeta        <- diag(as.matrix(X) %*% rep.col(beta,nrow(X)))
  if (method == 'probit'){
    scalefactor.PEA <- dnorm(sum(avg*beta)) # Apply the normal distribution
    scalefactor.APE <- mean(dnorm(xibeta), na.rm = TRUE)
  }
  if (method == 'logit'){
    scalefactor.PEA <- dlogis(sum(avg*beta)) # Apply the logistic distribution
    scalefactor.APE <- mean(dlogis(xibeta), na.rm = TRUE)}
  return(list(scale.PEA = scalefactor.PEA, scale.APE = scalefactor.APE, model = method))
}

X_nowhite    <- cbind(1, hrat , obrat , loanprc , unem , male , married 
                      , dep , sch , cosign , chist , pubrec , mortlat1 , mortlat2 , vr)
avg          <- apply(X_nowhite, MARGIN = 2, mean, na.rm = TRUE) # skip missing variables
probit_PartialEffect<- pnorm(sum(beta.probit[-2]*avg) + beta.probit[2]*1) -
                       pnorm(sum(beta.probit[-2]*avg))
logit_PartialEffect <- plogis(sum(beta.logit[-2]*avg) + beta.logit[2]*1) -
                       plogis(sum(beta.logit[-2]*avg))

# Estimate the OLS model with all control variables
olsnew       <- lm(approve ~ white + hrat + obrat + loanprc + unem + male + married +
                    dep + sch + cosign + chist + pubrec + mortlat1 + mortlat2 + vr)
ols_PartialEffect   <- olsnew$coefficients[2]


# Compare the Partial Effects of white across model specifications
PE           <- cbind(probit_PartialEffect, logit_PartialEffect, ols_PartialEffect)
colnames(PE) <- c('Probit', 'Logit', 'OLS')
xtable(PE, caption = 'Partial Effects', label = 'scale')
#print(xtable(PE, type = "latex", caption = "Partial effects"), 
#      file = paste(pwd,"/","output/tables/reg_out_ex_2_c_d_2.tex", sep=""))
detach(loans)



###############
# Question 3
###############
graphics.off() # Reset graphic output parameters
library(readxl)
library(forecast)
library(zoo)
library(xts)
library(fBasics)
library(broom)
library(xtable)
library(tseries)
dD <- read_excel("data/data_assignment2.xlsx",
                 sheet = 'stock_daily')

###############
# 3 a) - Daily
###############
r  <- log(dD$EWRET+1) # Log returns
rC <- log(cumprod(dD$EWRET+1)) # Log-Compounded

# Need to divide sample into estimate and evaluation sample
e1 <- which(dD$DATE==19970102) # Start date
e2 <- which(dD$DATE==20111130) # End date
r  <- log(dD$EWRET[e1:e2]+1)
y  <- log(dD$EWRET[(e2+1):nrow(dD)]+1)

# Setup export parameters for the graphs
png(filename=paste(pwd,"/","output/graphs/3_a.png", sep=""), 
    units="in", width=5.5, height=8, pointsize=12, res=300)
par(mar=c(4, 4,3,1), mfrow=c(2,1), xpd=F, new=FALSE)
# Plot the ACF and PACF
Acf(r, lag.max = 22, main="Log returns (EWRET)")
Pacf(r, lag.max = 22, main="")
dev.off()
# Both ACF and PACF die out gradually from the first to the third
# lag.  

###############
# 3 b) Estimate
###############

####################################################
# Simple Way using pre-programmed selection packages
####################################################

modA <- auto.arima(r, d = 0, max.p=5, max.q=5, max.order = 10, ic="aic", 
                   stepwise = FALSE, #  Search for all possible combinations from ARMA(0,0) to (5,5)
                   approximation = FALSE, # Also, shut down approximation to be accurate.
                   trace = TRUE) # Can trace the searching process
modB <- auto.arima(r, d = 0, max.p=5, max.q=5, max.order = 10, ic="bic",
                   stepwise = FALSE, approximation = FALSE,
                   trace = TRUE) 

# -> Both select ARIMA(3, 0, 3) as the best fitted model

##########################################################
# Own Control: we could also use self-defined ic functions
##########################################################

# Note that the information criteria uses the log likelihood (ll), which
# can be different up to a constant, but the relative values should have
# the same order.
fIC <- function(e, K) {
  # e: Residuals of the model
  # K: Total numbers of parameters K = p+q (+ constant)
  N <- length(e)
  ll <- log(mean(e^2)) # log( t(e) %*% e / N )
  aic <- ll + 2*K / N
  bic <- ll + (log(N)*K) / N
  return(list(aic=aic, bic=bic))
}

# Different calculation but same result
fIC2 <- function(ll, K, N) {
  # ll: Maximum log-likelihood of the model fit
  # K: Total numbers of parameters K = p+q (+constant)
  aic <- -2*ll + 2*K
  bic <- -2*ll + log(N)*K
  return(list(aic=aic, bic=bic))
}


# Setup the possible options for the order = c(p, 0, q)
model_search     <- as.matrix(expand.grid(0:5, 0,  0:5)) # Models that will be tested
result           <- matrix(NA, nrow=nrow(model_search), ncol=2) # Pre-allocate memory
colnames(result) <- c("AIC", "BIC")

# Estimate each model in model_search, and save the IC scores for each
for(i in 1:nrow(model_search)) {
  mod <- arima(r, order=model_search[i,]) # Estimate model at row 'i'
  # Perform the fiC functons:
  tmp <- fIC(e=mod$residuals, K=sum(model_search[i,])+1) # fIC are using ic formulas from class
  #tmp <- fIC2(ll=mod$loglik, K=sum(model_search[i,])+1, N = mod$nobs) # fIC2 gets a similar value of aic, bic with auto.arima
  result[i,] <- c(tmp$aic, tmp$bic) # Save to output matrix
  print(i) # Just to see that it actually progresses (good practice if code need to run for long time)
}

model_search[which.min(result[,1]),] # Find index of minimum AIC
model_search[which.min(result[,2]),] # Find index of minimum BIC

# Both select the same model ARMA(3,3)
# This is in consistent to auto.arima. 
m1 <- arima(r, order = c(3,0,3))
summary(m1)

# Ljung Box Test
library(portes)
LjungBox(m1, lags = 10)


###############
# 3 c) Forecast
###############
# Alternatively, you could also use plot(forecast, include = 60, ...) or autoplot(forecast, include = 60, ...)
# But the date format in x axis is confusing and I did not find a good way to fix it.
# So I use a simple way to plot as following:

# Use a data.frame to also include Date as a variable
df_forecast <- data.frame(Date = strptime(dD$DATE, f <- "%Y %m %d", tz = "GMT"), 
                          LogEWRET = log(dD$EWRET+1))

# Restrict the sample to the required period
df_forecast <- df_forecast[e1:(e2+21),] 

# Obtain the forecast results
f1 <- forecast(arima(r, order = c(3,0,3)), 21)

# Get the predicted values, as well as the upper and lower limits of the 
# prediction interval
df_forecast$Predict <- c(df_forecast$LogEWRET[1:(e2-e1+1)], f1$mean)
df_forecast$Upper   <- c(rep(NA, e2-e1+1), f1$upper[,2])
df_forecast$Lower   <- c(rep(NA, e2-e1+1), f1$lower[,2])

# Forecast estimates for the subsets
dff_subset          <- df_forecast[seq(1,e2-e1+1,by=100),]
pred_subset         <- df_forecast[(e2-e1+2):nrow(df_forecast),]
ylimit              <- range(pred_subset$Upper,pred_subset$Lower, finite = TRUE)
xlimit              <- range(df_forecast$Date[3700: nrow(df_forecast)])

png(filename=paste(pwd,"/","output/graphs/3_c_forecast.png", sep = ""))
par(mfrow = c(2,1))
# Plot the log returns against the date
plot(LogEWRET ~ Date, data = df_forecast[3700: nrow(df_forecast),], 
     type ="l", col="red", 
     xlim = xlimit, 
     ylim = ylimit, 
     xaxt = "n",
     ylab = "LogEWRET",
     lwd  = 2)
# Shade the area between the upper and lower bound for the prediction period
polygon(c(pred_subset$Date,rev(pred_subset$Date)), 
        c(pred_subset$Upper, rev(pred_subset$Lower)),
        col=rgb(0,0,0.6,0.2), border=FALSE)
# Input the forecast estimates
par(new=TRUE)
plot(Predict ~ Date, data = df_forecast[(e2-e1+2): nrow(df_forecast),], type="l", 
     ylim = ylimit,  
     xlim = xlimit,
     xlab = "", ylab = "",
     col ="blue",
     xaxt = "n",
     yaxt = "n",
     lwd = 2)
axis.POSIXct(side = 1, at = df_forecast$Date[3700: nrow(df_forecast)], 
          format = '%Y%m%d')
dev.off()


## Define accuracy measures
# Get the predicted and actual values
yhat <- pred_subset$Predict
y    <- pred_subset$LogEWRET

# Define accuracy measures
MSE  = mean((yhat - y)^2) # Mean squared error
MAE  = mean(abs(yhat - y)) # Mean absolute error
MAPE = 100* mean(abs(yhat/y - 1)) # Mean absolute percentage error

# Different definitions of Symmetric MAPE (or Adjusted MAPE)
SMAPE1 = 100* mean(abs(yhat - y)/((abs(yhat) + abs(y))/2)) # [0, 200%] SMAPE
SMAPE  = 100* mean(abs(yhat - y)/((abs(yhat) + abs(y))))# [0, 100%] SMAPE

# Gather the results
xtable(cbind(MSE, MAE, MAPE, SMAPE), digits = c(4,4,2,2,2),
       caption = "Forecasting Accuracy", label = "fc",
       file = paste(pwd,"/","output/tables/out_ex_3_c.tex", sep=""))


###############
# 3 d)
###############

# i)
# Get the respective residual series
resAbs <- abs(m1$residuals) # Absolute residuals
resSq  <- m1$residuals^2 # Squared residuals
resmat <- cbind(resAbs, resSq)

# Construct the summary statistics
tab_3d <- basicStats(resmat)[c(13, 15:16, 3:4),]

# Note:basicStats() in package fBasics gives excess kurtosis
# kurtosis() in package moments gives kurtosis

xtable(tab_3d,
       file = paste(pwd,"/","output/tables/reg_out_ex_3_d.tex"))


# ii)
png(filename=paste(pwd,"/","output/graphs/3_d.png", sep=""), units="in", width=5.5, height=8, pointsize=12, res=300)
par(mar=c(4, 4,3,1), mfcol=c(2,2), xpd=F, new=FALSE)
Acf(resAbs, lag.max = 22, main="Absolute value of residuals")
Pacf(resAbs, lag.max = 22, main="")
#dev.off()

#png(filename="3.d_sq.png", units="in", width=5.5, height=8, pointsize=12, res=300)
#par(mar=c(4, 4,3,1), mfrow=c(2,1), xpd=F, new=FALSE)
Acf(resSq, lag.max = 22, main="Squared residuals")
Pacf(resSq, lag.max = 22, main="")
dev.off()


# iii)
# Advantages of using ur.df: 
# a) You are free to pick models: non-constant, constant(drift), or trend models
# b) You are free to let the function endogenously select lags 
# under a certain information criteria: 'selectlags = '

# IF we use the same fixed order of lags, adf.test and ur.df with trend model
# should give you the same result. Here, I use nlag = 15 (which corresponds)
# to the default lag size in case nothing is selected for the ADF case.
res1 = adf.test(resSq)
res2 = ur.df(resSq, type = 'trend', lags = 15)
xtable(data.frame(t_adf.test = res1$statistic,
                  t_ur.df = res2@teststat[1]),
       caption = 't-statistics of Dickey-Fuller Test from different functions')
# Reject Non-Trend-Stationary => Trend-Stationary

# iv)
mAbs <- auto.arima(resAbs, 
                   max.p=5, max.q=5, max.order = 10, 
                   ic="bic", 
                   stepwise = FALSE, # To search for all possible combinations from ARMA(0,0) to (5,5)
                   approximation = FALSE, # Also, shut down approximation to be accurate.
                   trace = TRUE)
mSq  <- auto.arima(resSq,
                   max.p=5, max.q=5, max.order = 20,
                   ic="bic",
                   stepwise = FALSE, 
                   #approximation = FALSE,
                   trace = TRUE)

# Optimal lag order:
stargazer(arima(resSq, order = c(2,1,10)))


tsdiag(mAbs) # Looks very good

png(filename=paste(pwd,"/","output/graphs/3_d_2.png", sep=""))
tsdiag(mSq) # Looks OKAY
dev.off()


###############
# Question 4
###############
graphics.off() # Reset graphic output parameters
library(readxl)
library(vars)
library(zoo)
library(lubridate)
library(stargazer)
library(tseries)
library(xts)
library(xtable)
library(fBasics)
library(broom)
rate <- read_excel("data/data_assignment2.xlsx",
                 sheet = 'rates')


# 1) VAR estimate
# Are the data stationary? 
# Clearly, eyeballing the dataset it is visible that the data series is non-stationary.
# This hypothesis is confirmed when performing an Augmented Dickey Fuller test.
dev.off()
plot(rate[,2:3])
plot(rate[,c(1,3)])
apply(rate[,2:3],2, adf.test)

# In order to make the series stationary perfrom first-differences
stationary_rate <- apply(rate[,2:3], 2, diff)
# Again perform the ADF test and note that the series are now stationary
apply(stationary_rate,2, adf.test) 

# Estimate the VAR model:
# Need to index and order the time series in order to apply a VAR regression
zoo_rate <- zoo(rate[,2:3], order.by = as_date(rate$DATE))
# Specify the VAR(2) model
var.rate <- VAR(zoo_rate, p = 2)
stargazer(var.rate[1])

# 2) Impulse Response Function
### Attention: I noticed that changing the specifications (e.g. ortho = TRUE/FALSE) has
### quite a large impact. 
# 1-year Treasury rate
var.rate.irf1 <- irf(var.rate, impulse = "X1YRRATE", response = c("X1YRRATE", "X5YRRATE"),
                     ortho = TRUE, n.ahead = 12,
                     boot = TRUE, ci = 0.95, runs = 1000)
# 5-year Treasury rate
var.rate.irf5 <- irf(var.rate, impulse = "X5YRRATE", response = c("X1YRRATE", "X5YRRATE"), 
                     ortho = TRUE, n.ahead = 12,
                     boot = TRUE, ci = 0.95, runs = 1000)

# Plot of 1-year Treasury rate
png(filename=paste(pwd,"/","output/graphs/irf1.png", sep=""))
plot(var.rate.irf1)
dev.off()

# Plot of 5-year Treasury rate
png(filename=paste(pwd,"/","output/graphs/irf5.png", sep=""))
plot(var.rate.irf5)
dev.off()

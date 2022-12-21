#####################################################
# Preparations
#####################################################
graphics.off()   # Shuts down all open graphics
rm(list = ls())  # Remove objects from the workspace
options(warn=-1) # Print warnings if they occur

# Set the working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################################################
# Question 1
#####################################################
# (a) Import the data (xlsx) in R
library(readxl)
dM <- read_excel("data/data_assignment1.xlsx", sheet = "stock_monthly")

#####################################################
# (b) Transform the simple returns into log returns
# (Recall: for x small -> log(1 + x) ~ x)
rM <- log(1 + dM[,2:6])

#####################################################
# (c) Plot the graphs of simple and log returns for all the series
yl <- range(rM, dM[,2:6]) # Assure that the axis limits are aligned
par(mar=c(5,4,4,5))

## 1) Plot the  returns of Coca Cola individually as an example
with(dM, plot(ts(dM[,2], frequency=12, start=c(1963,12))
              , col ="red3"
              , lwd = 2                       # Adjust the line width for better visibility
              , ylab="Simple-Returns"         # Label of y axis
              , ylim=yl                       # Adjust the limits of the y-axis for better comparability
              , main=paste(colnames(dM[2])))) # Put the column name as the title
par(new=TRUE)                                 # Assure to query new graphical parameters
with(rM, plot(ts(rM[,1], frequency=12, start=c(1963,12))
              , axes=F
              , xlab=NA
              , ylab=NA
              , ylim=yl))
# Get the log return labels
axis(side = 4)  
mtext(side = 4, line = 3, 'Log-Returns',lwd = 2)
# Define the legend and denote its location
legend("topright", legend = c("simple", "log"),
       lty = c(1,1), col = c("red3", "black"), cex = 0.5)


## 2) Plot all returns in subgraphs together
# Loop over all returns in the return matrix
par(mar=c(4, 4,1,1), mfrow=c(ncol(rM),1), xpd=F, new=FALSE)
for(i in 1:ncol(rM)) {

  # Point plor for the simple (level) returns
  plot(ts(dM[,(i+1)], frequency=12, start=c(1963,12))
       , ylim=yl, ylab=" ", size = 2, pch = 19
       , main=paste(colnames(rM[i]))  # Get the column name as plot header
       , col = 'red3', lty = 1
       , type = 'b') 
  # Line plot for the log returns
  lines(ts(rM[,i], frequency=12, start=c(1963,12))
       , ylab=" ", pch = 18, size = 2, type = 'l', lty = 2
       , col = 'blue'
       , ylim=yl)
  # Define the legend and denote its location
  legend("topright", legend = c("Simple-Returns", "Log-Returns"),
          col = c("red3", "blue"), lty=1:2,  cex = 0.4,  pch = 1)
}

#####################################################
# (d) Plot the scatterplots for the log return series.
par(mfrow= c(5,1)) # Set the graphical query parameters
col = c("red", "orange", "black",
        "green", "blue")
for(i in 1:ncol(rM)) {
plot(ts(rM[,i], frequency=12, start=c(1963,12))
     , xlab="Date"                    
     , ylab="Log-Returns"
     , ylim=yl
     , type= "p"
     , pch = 20
     , col = col[i]
     , bg = "transparent"
     , main=paste(colnames(rM[i])))
}

######################################################
# (e) Compute the sample mean, variance, skewness, excess
# kurtosis, minimum and maximum of log returns.

# The  "fBasics" package is a collection of functions to explore and  investigate 
# basic properties of financial returns and related quantities.
library(fBasics)
desc <- basicStats(rM)
# Select the relevant statistics
print(desc[c(7, 13, 15:16, 3:4),]) 

#####################################################
# (f) Are the sample mean, skewness and excess kurtosis 
# of log returns statistically different from zero?
library(moments)                                      # Use the moments library for testing
mean_test     <- apply(rM, MARGIN = 2, t.test)        # test if mean = 0, MARGIN = 2 for columns
skewness_test <- apply(rM, MARGIN = 2, agostino.test) # test if skewness = 0, MARGIN = 2 for columns
kurtosis_test <- apply(rM, MARGIN = 2, anscombe.test) # test if kurtosis = 3 <-> tests if excess kurtosis = 0, MARGIN = 2 for columns

# Define a function to gather all the test results in a nice way
summary_test  <- function(test_above, momentname){
  # Define the number of tests
  n <- length(test_above)
  test_stats <- data.frame(samplestats = numeric(), z = numeric(), p_value = numeric())
  # Specify th number of test assets (in our case 5)
  for (i in (1:n)){
    # Test moments other than the mean 
    if (momentname != 'mean'){
      # Store the test statistics for moments other than the mean
      test_statsnew           <- data.frame(t(c(as.vector((test_above[[i]]$statistic)),
                                                test_above[[i]]$p.value)))
      # Attach the asset name as the row name
      rownames(test_statsnew) <- colnames(rM[i])
      # Attach the name of the test statistic as the column name
      colnames(test_statsnew) <- c(momentname, 'z', 'p_value') 
    }
    # Test the mean
    if (momentname == 'mean'){
      # Store the test statistics for the mean
      test_statsnew           <- data.frame(test_above[[i]]$estimate, 
                                            test_above[[i]]$statistic, 
                                            test_above[[i]]$p.value)
      # Attach the asset name as the row name
      rownames(test_statsnew) <- colnames(rM[i])
      # Attach the name of the test statistic as the column name
      colnames(test_statsnew) <- c(momentname, 't', 'p_value') 
    }
    # Bind the test statistics together
    test_stats                <- rbind(test_stats, test_statsnew)
  }
  # Return the results
  return(test_stats)
}

# Execute the function
summary_test(mean_test,     'mean')
summary_test(skewness_test, 'skew')
summary_test(kurtosis_test, 'kurt')

## Solution:
# Yes, except for the skewness of IBM, all statistics 
# are statistically significantly different from zero.

#####################################################
# (g) Obtain the histograms of the returns and compare 
# them with normal and Student distributions that have 
# the best fit of the empirical distribution.

library(MASS) # Needed to estimate df in the t-distribution

## Create output (will output one column at the time)
# Loop over all collumns of the log test assets:
for(i in 1:ncol(rM)) {
  # Normalize the returns to properly fit the normal and student-t distribution
  x <- (rM[,i] - mean(rM[,i]))/sd(rM[,i]) 
  
  # Define the graph settings:
  par(mar=c(4, 4,1,1), mfrow=c(1,1), xpd=F, new=FALSE)
  
  # Plot the empirical histogram [# bins: 100]
  h    <- hist(x, breaks=100, col="gray"
            , xlab="Log return"
            , main=paste(colnames(rM[i]), " (Normalized)"))
  
  ## Add normal density
  # Create data vector of length 80 between the empirical minimum and maximum of x
  xfit <- seq(min(x),max(x),length=80)
  # Create a normal ditribution with the same mean and std as the empirical sample
  nfit <- dnorm(xfit,mean=mean(x),sd=sd(x))
  # Scale up the distribution
  nfit <- nfit*diff(h$mids[1:2])*length(x)
  lines(xfit, nfit, col="blue", lwd=2)
  
  ## Add student-t density
  # Fit the t-distribution to the data
  mod  <- fitdistr(x, densfun="t")
  tfit <- dt(xfit, mod$estimate[3])
  # Scale up the distribution
  tfit <- tfit*diff(h$mids[1:2])*length(x)
  lines(xfit, tfit, col="red", lwd=2)
  
  # Add the legend
  legend("topleft", col=c("blue", "red"), 
         legend=c("Normal", 
                  paste("t (", round(mod$estimate[3], digits=1), 
                        " df)", sep="")), pch=20)
  
    # Can be used to output all histograms
  #dev.new()
}

# Further check for deviations from normality.
# For deviation from normality -> Plot the quantiles of the index against the 
# quantiles implied by normality
qqnorm(rM[,4])
qqline(rM[,4], col="red")

## Jarque-Bera Test for the indices
JB_VWRET  <- jarqueberaTest(rM[,4])
JB_VWRET
JB_EWRET  <- jarqueberaTest(rM[,5])
JB_EWRET

# Comment
# Given the (very) low p-value for both indices we can reject H_0 of normality. The
# JB statistic is higher for the equally weighted index suggesting that
# it deviates even more from normality. 

# Optional: Better way to "test" normality (JB is a terrible test of normality):
# -> Use Shapiro-Wilks instead.
shapiro.test(rM[,2])


#####################################################
# (h) Estimate a CAPM regression for Coca-Cola, GE and IBM. You can assume the
# monthly risk free rate is 0% for this part. What are the alphas ? What are the
# betas ? Discuss the statistical significance of the estimates.

# Recall CAPM: E[R_i] = R_f + beta_i(E[R_m] - R_f) ;   In our case R_f := 0
fit_cocacola      <- lm( COCACOLA ~ VWRET, data = rM)
fit_GE            <- lm( GE  ~ VWRET, data = rM)
fit_IBM           <- lm( IBM ~ VWRET, data = rM) 
library(stargazer)
stargazer(fit_cocacola, fit_GE, fit_IBM,type = 'text')

## Comment:
# Conclusion: As the p value for betas are all zeros, it means betas 
# are all siginificantly different from zero. Actually, all betas are
# positive, which indicates that all three stock returns are positively 
# correlated with the market portfolio. In terms of alphas, it is 
# interesting that Coca-Cola earns a significantly positive abnormal return. 
# The p-values for alpha_ge and alpha_ibm are very high so that we can not 
# reject the zero-value null hypothesis. 


## Clear all existing variables to save memory space
rm(list=ls())





######################################################
# Question 2
######################################################

######################################################
#(a) Import the data in R
# Input Stata file
library(haven)
pension          <- 
  read_dta("data/pension.dta")

######################################################
#(b) How many single-person households are there in the dataset?
sum(pension$fsize == 1) 
# There are 2017 single-person hhs in the dataset


######################################################
#(c) Use OLS to estimate the model for the case of single-person households
pension_single  <- pension[pension$fsize == 1,]
fit_single      <- lm(nettfa ~ inc + age, data = pension_single)
fit_single      <- summary(fit_single)
fit_single

## Comment:
# An increase in the annual family income of USD 1000 yields, on average and ceteris paribus,
# an increase in the net financial wealth of around USD 800 (note that both wealth and income 
# are both in thousands of USD. Similarly, an increase in the survey respondent's age by one 
# year leads, on average and ceteris paribus, to an increse in the net financial wealth of around 
# USD 843



######################################################
#(d) Does the intercept from the regression in part (c) have an interesting meaning?

# Answer: 
# The intercept of -43039.8 literally means that a single-person with no income and zero 
# age has a predicted net total financial asset of -43039.80 dollars. Obviously this makes
# little sense as we would basically focus on the net financial wealth of a new-born.
# Technically, the intercept term ensures that the mean of the residuals equals  zero and therefore
# there is no particular interest in the coefficient per se.

#####################################################
#(e) Find the p-value for the test H0 : beta_2 = 1 against H1 : beta2 < 1. Do you reject H0 at
# the 1% significance level? 
# One-sided t-test (!)
n = nrow(pension_single)                 # Get the total number of individuals
k = 2                                    # Get the coefficient estimates
beta2_null     <- 1                      # beta2 under H_0
beta2_se       <- coef(fit_single)[3,2]  # Get the standard error of age
beta2_estimate <- coef(fit_single)[3,1]  # Get the coefficient estimate of age
t              <- (beta2_estimate - beta2_null)/beta2_se # Compute the t-statistic
# Print the t-statistic
print(t)
pvalue_lower   <- pt(-abs(t), df = n - k - 1)   # n-k-1 degrees of freedom
pvalue_lower
# pvalue_lower = 0.04 gives the probability that we would oberserve
# a test statistic less than t =  -1.71 under the null hypothesis. 
# Put differently, p values reflects the probability that one observes
# a more extreme t statistic if the null hypothesis is true. 
# Since the pvalue_lower = 0.04 > alpha = 0.01, we can not reject the null
# that beta_2 = 1 at the 1 % significance level.


#####################################################
#(f) Find the p-value for the test H0 : beta2 = 1 against H1 : beta2 != 1. 
# Do you reject H0 at the 1% significance level? Note: Now we have a two-sided test.
# 99% confidence interval 
pvalue_twoside <- 2*pvalue_lower
pvalue_twoside

# Conclusion is the same as in part e): We can not reject the H_0 that beta_2 = 1

####################################################
#(g) If you run a simple regression of nettfa on inc, 
# is the estimated coeffcient on inc much different from 
# the estimate in part (c)? Why or why not?
fit_single_inc<- lm( nettfa ~inc  , data = pension_single)
fit_single_inc<- summary(fit_single_inc)
fit_single_inc

# Beta_1 (= 0.82) here is only marginally higher than the one from 
# the initial regression fit_single (beta_1 = 0.80). 
# This may due to the positive correlation between inc and age. 
# Ommiting the variable "age" leads to a small upward bias in the 
# coefficient of inc.  

# Note: beta_hat = beta_pop + (X'X)^(-1)(X'e) ~ beta_pop + Cov(X,e)/Var(X)
# Ommiting age from the model implies that "age" is subsumed in the error term. 
# Therefore, provided that age is correlated with income this would lead to a correlation
# between the error term and the regressor.

###################################################
# (h) Re-estimate the model in (c) by adding inc2 and age2 as additional regressors
fit_all_ply   <- lm(nettfa ~ inc + age + incsq + agesq + fsize, data = pension)
fit_all_ply   <- summary(fit_all_ply)
fit_all_ply
# Finding:
# Interestingly, now the effect of age is negative while the effect of the squared age term is
# positive. This hinges towards the fact that at youg age the total effect of an increase in age
# is negative, i.e. -1.258 age + 0.0263771 age^2 < 0. For higher ages the second term will start to dominate
# s.t. the marginal effect of becoming one year older on net financial wealth is in fact positive.


###################################################
# (i)Calculate the F-test of the restriction 
# H0 : beta_3 = 0 & beta_4 = 0. Do you reject the null hypothesis? 

# 1) Fit the restricted model:
fit_all_r   <- lm( nettfa ~ inc + age + fsize, 
                     data = pension)
fit_all_r   <- summary(fit_all_r)
# Get the R^2 from the restricted model
r2_r <- fit_all_r$r.squared

# 2) Fit the unrestricted model:
fit_all_ply   <- lm( nettfa ~ inc + age + incsq + agesq + fsize, 
                     data = pension)
fit_all_ur    <- summary(fit_all_ply)

# Get the R^2 from the unrestricted model
r2_ur <- fit_all_ur$r.squared
# Number of individuals in the sample
n  <- nrow(pension)
# number of restrictions 
q  <- 2 
# number of independent variables in the unresitricted model
k  <- 5 

# F statistics
F_i <- ((r2_ur - r2_r)/q)/((1-r2_ur)/(n-k-1))
print(F_i)  
# F = 147.3292

## Alternative: A different formular of F statistics based on the sum of squared residuals
# SSRu <- anova(lm( nettfa ~ inc + age + incsq
#                   + agesq + fsize, data = pension))[6,2]
# SSRr <- anova(lm( nettfa ~ inc + age + fsize, 
#                   data = pension))[4,2]
# F_i <- ((SSRr - SSRu) /q)/(SSRu / (n - k_f - 1))

# Obtain the critical value using the F distribution
pf(F_i, df1 = q, df2 = n - k - 1, lower.tail = FALSE) 
# p value = 1.027369e-63
# -> We can reject at 1% significence level the hypothesis that beta_3 = beta_4  = 0

###################################################
#(j)Rescale the variable inc in (2) by dividing it by 10. Re-estimate (2) with the
# rescaled variable.

# Note: I() prevents the formula-interface from interpreting the argument, so it gets passed along 
# instead to the expression-parsing part.
fit_all_rescale <- lm(nettfa ~ I(inc/10) + age + I(inc^2) + I(age^2) + fsize, 
                      data = pension)
fit_all_rescale <- summary(fit_all_rescale)
fit_all_rescale
## Comment: The only change is new beta_1 = old beta_1 * 10. Other statistics such as
#  the standard errors do not change. 

###################################################
# (k) Test the original model in (1) for heteroskedasticity using the Breusch-Pagan test.
# Null hypothesis: Var(u|X) = sigma^2 = constant (i.e. homoskedasticity)

# Step 1: Obain the OLS squared residuals u_hat^2
u_hat_sqd <- fit_single$residuals^2
# Step 2: Run the regression: u_hat^2 ~ d_0 + d_1*x_1 + d_2*x_2 + ...+ v
fit_u     <- lm(u_hat_sqd ~ pension_single$inc + pension_single$age)
# Step 3: Form F statistics of the joint test of d_1, d_2, ... and compute the t value
# if the p value is small, reject H0
summary(fit_u) 
# F-statisti 5.994 on 2 and 2014 DF,  p-value: 0.002537
# So we reject H0!

# Alternative: Use pre-programmed BP test:
library(lmtest)
bptest(fit_single) 

# Note: If studentize = F is selected the results do qualitatively not change.
bptest(fit_single, studentize = F) 

# Additional note about studentization:
# Using the studentized version provides more robust test results. Essentially,
# Koenker (1981) ["A Note on Studentizing a Test for Heteroscedasticity."] 
# outlines a way to obtain asymptotically correct significance levels for a 
# reasonably large class of distributions for the error term. This is essentially
# motivated by issues regarding the asymptotic power of the BP test.


###################################################
# (l) Estimate the original model in (1) with heteroskedasticity-robust (e.g., White) 
# standard errors.
library(sandwich)
# Compute the hetereoscedastic robust standard error
fit_single <- lm(nettfa ~ inc + age, data = pension_single)
coeftest(fit_single, 
         vcov = vcovHC(fit_single,type="HC")) # HC/HC0 is White's (1980) estimator

###################################################
# (m) Standardize all the variables in the model (1). Re-estimate (1) with the standard-
# ized variables.

# Note: The scale function centers around the sample mean and divides by the sample standard
# deviation. 
pension_single_stan <- scale(pension[, c("nettfa", "inc", "age")])
pension_single_stan <- data.frame(pension_single_stan)
fit_single_stan     <- lm(nettfa ~ inc + age, data = pension_single_stan)
fit_single_stan     <- summary(fit_single_stan)
fit_single_stan

# Compare to the coefficient estimates obtained in exercise 2c)
summary(fit_single)

# Answer: Centering y makes the intercept estimate shrink to zero. 
# Scaling y and x leads to different coefficient estimates. In particular,
# the coefficients now measure how many standard deviations the dependent variable, y, 
# changes in response to a one standard deviation change in the independent variable, x.
# The standarization will not affect t-statictics, p values, R-squared, or
# F-statistics. Therefore, the explanatory power of the standandized model is the 
# same with respect to the non-standandized one. The only change is the interpretation of 
# the coefficients, as well as its corresponding standard errors.

## Clear all existing variables to save memory space
rm(list=ls())




# Question 3
####################################################
# (a) Estimate the model by OLS with log values
ceo_salary <- 
  read_dta("data/ceo_salary.dta")

# Fit a linear OLS model using standard (i.e. non-adjusted standard errors):
fit_ceo <- lm(lsalary ~ lsales + lmktval + ceoten + ceotensq, 
              data = ceo_salary)
summary(fit_ceo)

###################################################
# (b) Report the count, mean, standard deviation, min and max of the 
# explanatory variables. 
# Why are most of the explanatory variables in natural logs? Explain.
desc_ceo         <- basicStats(ceo_salary[,c( "salary", "sales", 
                                              "mktval", "ceoten", 
                                              "ceotensq")])
desc_ceol        <- basicStats(ceo_salary[,c( "lsalary", "lsales", 
                                              "lmktval", "ceoten", 
                                              "ceotensq")])
desc_ceol[c(1, 7, 14, 3, 4),]

## Comment:
# Reason 1:
# In a log-log model, beta measures the elasticity of y with respect to x. 
# The coeffecients of lsale thus is the estimated elasticity of salary 
# with respect to sales.
# It implies that a 1% increase in firm sales increases CEO salary
# by about 0.16% - the usual interpretation of an elasticity.
## Reason 2:
# Statitically, taking the natural log of a value can also reduce 
# the variation caused by extreme values. It can make
# highly skewed distributions less skewed and thus reduces the undue influence of
# outliers.
## (Reason 3):
# Log-transformations are often analytically convenient in the sense that they 
# are more tractable when deriving FOCs, etc.

###################################################
# (c) Re-estimate the model with White standard errors and compare the t-stats.
coeftest(fit_ceo, vcov = vcovHC(fit_ceo, type = "HC"))
# t-statics of lsalary and lmktval are larger while the others are smaller.
# It is important to note that with hc-robust standard errors, the standard errors
# can change in both ways as described here (http://econ.lse.ac.uk/staff/spischke/mhe/josh/Notes%20on%20conv%20std%20error.pdf)

# For comparison:
summary(fit_ceo)

###################################################
# (d) Obtain the residuals and standardize them.
resid_ceo        <- fit_ceo$residuals
# How many of the residuals are above 1.96:
print(sum(abs(scale(resid_ceo)) > 1.96))


# Note: The function pnorm returns the integral from −∞ to q of the pdf of the 
# normal distribution where q is a Z-score. Try to guess the value of pnorm(0)
# If lower.tail is set equal to FALSE then pnorm returns the integral from q to ∞
norm_draw        <- 2*pnorm(2,lower.tail = FALSE) * 177 
norm_draw

# Answer: Around 8 draws are above 1.96 in absolute value. If the standardized residuals 
# were i.i.d draws from a standard normal distribution, about 8.05 draws are in expectation 
# above 2 in absolute value with 177 draws (and 8.85 are expected to be above 1.96).
# The standarized variable is very close to a normal distribution. 

# Visually check this using a Q-Q plot:
qqnorm(resid_ceo)
qqline(resid_ceo, col="red")

###################################################
# (e) Add the dummy college and the interaction term college*lsales to the model in (2).
# Estimate this model. How you do interpret the coeffcient on the interaction term?
fit_ceo_int <- lm(lsalary ~ lsales + lmktval + ceoten + ceotensq + 
                college + college:lsales, data = ceo_salary)
summary(fit_ceo_int)

# The coefficient on the interatiion term helps to analyze the
# partial effect of college and lsales on a CEO's salary. 
# Note that now beta(lsales) measures the effect of lsales
# when college = 0, which is insigificant. This implies that CEOs without 
# a college degree do not have a correlation between the value of sales  and their salary. 
# beta(lsales) + beta(lsales:college)
# measures the effect of lsales when college = 1, i.e. the CEO attended a college.
# beta(lsales:college) = 0.47 hence implies that for every 1% increase in firm sales, 
# the salary of CEOs with a college degree increases by 0.47% more than CEOs without
# a college degree.  

# Note of caution: Only very few CEOs did NOT attend college after all.*
##################################################
# (f) Let us suppose you found out that the variable lsales has measurement error 
# because sales have been overstated, and the true value of the sales is 
# 10% smaller (e.g., reduce each value of lsales by 10%). 
# Call this variable lsales_adjusted. Re-estimate the model in (2) with 
# lsales_adjusted as an explanatory variable instead of the original lsales.
ceo_salary$lsales_adjusted <- ceo_salary$lsales*(1-0.1)
fit_ceo_adjusted <- lm(lsalary ~ lsales_adjusted + lmktval + ceoten + ceotensq, 
                       data = ceo_salary)
gamma_1    <- coef(fit_ceo_adjusted)[2]
beta_1     <- coef(fit_ceo)[2]
se_gamma_1 <- sqrt(diag(vcov(fit_ceo_adjusted)))[2]
se_beta_1  <- sqrt(diag(vcov(fit_ceo)))[2]

output_esti <- cbind(rbind(gamma_1, se_gamma_1), 
                     rbind(beta_1, se_beta_1))
rownames(output_esti) <- c('estimate', 's.e.')
output_esti

# Rescaling test
all.equal(as.vector(output_esti[,1]) ,
          as.vector(output_esti[,2]/0.9))
# Answer: The only change in the new model is that the esimate and standard error of
# lsales are rescaled. Mathematically, gamma_1 = 1/0.9*beta_1 and 
# se_gamma_1 = 1/0.9*se_beta_1

##################################################
# (g) Drop all observations where the standardized residual in point (d) is greater than
# 2 in absolute value. Re-estimate the model in (2). Do you notice any difference in
# the OLS estimation?
drop_idx               <- abs(scale(resid_ceo)) > 2 
# sum(drop_idx) = 7, seven observations to drop  
# list out the row index of observations to be dropped
which(drop_idx == TRUE)
ceo_salary_afterdrop   <- ceo_salary[!drop_idx,]
fit_ceo_afterdrop      <- lm(lsalary ~ lsales + lmktval + ceoten + ceotensq, 
                             data = ceo_salary_afterdrop)
summary(fit_ceo_afterdrop)


# You coud also try: 
par(mfrow = c(2, 2))
plot(fit_ceo) 
# From the plot of residuals, you may notice that which(drop_idx == TRUE)
# indeed list out all extreme values 



# Compare fit_ceo_afterdrop with the initial regreesion fit_ceo, we can find that
# the estimate for lmaktval is strengthened after droping extreme values.
# The magnitude of other estimates is undermined. The coefficient 
# of ceotensq becomes insignificant. Meanwhile, the higher R-squared value in
# fit_ceo_afterdrop implies that our model explains variation in the lsalary variable
# better after excluding extreme values. 



# Question 4:
##################################################
# (a) Is it a balanced or unbalanced panel?

# Alternative to read in the data:
local_returns <- read_excel("data/local_returns.xlsx")
library(haven)
local_returns          <- 
  read_dta("data/local_returns.dta") 


# By attaching a dataFrame (or list) to the search path it is possible to refer to the variables 
# in the data frame by their names alone, rather than as components of the data frame 
# (e.g., in the example below, height rather than women$height).
library(plyr)
attach(local_returns)
obs_byfirm             <- ddply(local_returns, .(permno), nrow)
colnames(obs_byfirm)   <- c("permno",  "NumObs")
summary(obs_byfirm)
hist(obs_byfirm$NumObs)

# Answer: Not a balanced panel as the number of 
# time-series observations across firms are not the same.

##################################################
# (b) Report average returns and volatilities of firms in the 20 cities 
# (e.g., summary statistics by city). 
# Do you see any differential returns amongst cities?
obs_bycity             <- ddply(local_returns, .(city), 
                                summarize, mean = mean(ret), 
                                sd = sd(ret))
obs_bycity

# There is substantial heterogeneity across cities w.r.t returns (minimum: 0.0059 vs.
# maximum of 0.0165). A similar pattern can be observed for the volatility.

#################################################
# (d) Estimate (3) using pooled OLS without dummies.
library(Matrix)
library(lfe)
library(lmtest)

pooled_ols            <- lm(ret ~ city_returns + indret, 
                              data = local_returns)
summary(pooled_ols)

pooled_ols_alt <- felm(ret ~ city_returns + indret)
summary(pooled_ols_alt)
#################################################
# (e) Test for heteroskedasticity in the residuals. What do you find?
bptest(pooled_ols, studentize = F) 
# As visible by the  p value well below 0.01 of the Breuch Pagan test, we conclude that
# heteroskedasticity is likely present in the given sample. I.e. we can reject the
# H_0 of homoskedasticity at the 1% level.

#################################################
# (f) Estimate (3) using pooled OLS with time fixed effects 
# (e.g., time dummies).

# lm with time fixed effects: use function "felm" 
# to Fit A Linear Model With Multiple Group Fixed Effects
pooled_time <- felm(ret ~ city_returns + indret | date, 
                    data = local_returns)
summary(pooled_time)

# Alternative using lm
pooled_time_lm <- lm(ret ~ city_returns + indret + factor(date),
                       data = local_returns)
summary(pooled_time_lm)

# Executing the above two specifications you notive two differences:
# 1) The felm() package does not give out the estimates of the dummy variables while
#    the lm() package with factor(date) specification does.
# 2) The computing power is  much longer for the second approach.

# Further note that the coefficient estimates for "city_returns" and "indret" are identical.
# Essentially, the felm() package "projects out" the fixed effects that we are not
# particularly interested per se (in some applications, e.g. AKM you would be interested
# in obtaining also the coefficient estimates for the fixed effects but our case here
# we simply want to control for unobserved heterogeneity). Therefore, using the felm()
# pacakge is particualrly suited for large datasets when the lm() + factor() specification
# is computationally too costly.

################################################
# (g) Estimate (3) using pooled OLS with both time and firm fixed effects.
pooled_timefirm <- felm(ret ~ city_returns + indret | date + permno, 
                        data = local_returns)
summary(pooled_timefirm)

# Overall the results do not change too much between the two sepcifications. 
# When controlling for firm fixed effects
# (i.e. factors that are idiosyncratic for each firm but constant across time) the coefficient
# of city returns is roughly 0.1706 and the coefficient on the returns of firms in the same 
# industry is roughly 0.8747. When additionally controlling for time trends that are
# identical across firms the results barely change. In addition, the results are both 
# qualitatively and quantitatively similar to the pooled OLS results from exercise 4d. 
# This finding can be taken as first evidence that the results are not 
# driven substantially by unobservable firm/time factors.


###############################################
# (h) Estimate (3) using pooled OLS with both time and firm fixed effects, 
# and with double clustering on both firm and time dimensions.
pooled_timefirm_cluster <- 
  felm(ret ~ city_returns + indret | date + permno | 0 |  date + permno, 
       data = local_returns)
summary(pooled_timefirm_cluster)

# It can clearly be seen that once we incorporate clustering on both the time and 
# the firm dimension the standard errors increase and hence the t-statistics decrease.
# Intuitively, clustering relaxes assumptions on the cross-sectional and temporal
# independence and hence produces more conservative standard errors.

###############################################
#(i) FE esitimator

# Only firm fixed effects
library(plm)
library(lfe)
FE_individual <-  plm(ret ~ city_returns + indret, data = local_returns, 
                      index = c("permno", "date"), effect = "individual", model = "within")
summary(FE_individual)

# Both time and firm fixed effects (optional)
#FE_twoway     <-  plm(ret ~ city_returns + indret, data = local_returns, 
#                      index  = c("permno", "date"), 
#                      effect =   "twoways", model = "within")
#summary(FE_twoway)

# Note: Here you need to use the plm() package with the "within" specification.
# As shown in the lecture the
# LSDV approach (as done above) is identical to the within fixed effect approach.
# However, when asked to estimate a fixed effect estimator (FE) estimator this
# is known to be the within-estimator. Hence, employing the lm/felm package here
# leads to the same results but is different from estimating a (within) fixed 
# effect regression. 


###############################################
#(j) FD estimator
FD <- plm(ret ~ city_returns + indret, data = local_returns, 
          index = c("permno", "date"), model = "fd")
summary(FD)

# Comparison: The FD estimator is equivalent to the FE estimator only when 
# T = 2. For a panel with longer time length, they will be different.
# Also, in plm(), the input for "index" will not matter for fd. As the FD 
# estimator is always first-order-differencing along the time dimension. 
# In our case, the FE and FD coefficient estimates are quite similar ( the FE model has
# a higher R2). 
# Pooled-OLS with fixed effects should have
# the same results with the (within) fixed effects model (FE_twoway = pooled_timefirm)

###############################################
#(j) FD estimator
stargazer(pooled_timefirm_cluster, FE_individual, FD, type = 'text',
          column.labels = c('Pooled Cluster','FE', 'FD'))


###############################################
# (l) generate a dummy variable dummy_SF.
dummy_SF <-as.numeric(city == 10)
# Or dummy_SF <- model.matrix(~factor(city == 10))
lm_dummy  <- lm(ret ~ city_returns + indret + dummy_SF:city_returns)
summary(lm_dummy)
  
# Gamma_1 = 0.024, is siginificantly different from zero only at the 
# significance level of 10%. 
  
# When dummy_SF = 0 it implies that a one unit increase in city_returns
# is associated (on average and c.p.) with a 0.21 unit increase in the return of firms 
# located in the same area (i.e. cities other than San Francisco). 
  
# Essentially, we are asking here whether the effect of other firms' returns in the same
# are is different depending on whether these other firms are located in San Francisco
# or not. This could be based on our intuition that firm returns are essentially driven 
# by other (influential) firms that are based in San Francisco. An equivalent could be 
# that we include a Sillicon Valley dummy because we think that firms there are very
# influential and hence have a large impact on firm returns.
  
# Technically, if the firm is located in San Fransisco, a one unit 
# increase in city-return can lead to 0.2102 + 0.0238 = 0.2340 units increase in firm return. 
# Given that the interaction term is not statisically significant at the 5% level
# we can not conclude that the effect of other firms' returns within  the same area 
# on firm returns differs depending whether these other firms are located inside
# or outside of San Francisco.
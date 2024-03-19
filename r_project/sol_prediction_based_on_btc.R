## csv to DataFrame ############################################################
sol <-read.csv("misc/sol_with_btc.csv") # read csv file

sol <- na.omit(sol) # omit nulls

## creates model with all variables ############################################
complete_model <- lm(formula = sol_close ~ .,
                     data = sol)

summary(complete_model) #summary of this model
# Multiple R-squared:  0.9333
# F-statistic p-value: < 2.2e-16
# note: some Betas do not have a good p-value of T. Need a stepWise!


## StepWise ####################################################################
stepwised_model <- step(object = complete_model,
                        k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

summary(stepwised_model) #summary of a stepwised model
# 4 great beatas

# Call:
#   lm(formula = sol_close ~ last_day_btc_high + last_day_btc_low +
#        last_day_btc_close + last_day_btc_marketcap, data = sol)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max
# -10.889  -2.153  -0.307   1.732  16.142
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)             1.588e+01  4.874e-01  32.582  < 2e-16 ***
#   last_day_btc_high       1.046e-03  1.881e-04   5.560 4.66e-08 ***
#   last_day_btc_low       -3.579e-04  1.810e-04  -1.978   0.0486 *
#   last_day_btc_close     -1.319e-01  2.518e-03 -52.389  < 2e-16 ***
#   last_day_btc_marketcap  7.034e-09  1.359e-10  51.752  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.661 on 446 degrees of freedom
# (1 observation deleted due to missingness)
# Multiple R-squared:  0.933,	Adjusted R-squared:  0.9324
# F-statistic:  1553 on 4 and 446 DF,  p-value: < 2.2e-16


## SF test ####################################################################
library("nortest")

sf.test(stepwised_model$residuals)
# Shapiro-Francia normality test
# data:  stepwised_model$residuals
# W = 0.93288, p-value = 7.477e-12


## heteroscedasticity test ####################################################
library("olsrr")

ols_test_breusch_pagan(stepwised_model)
# Breusch Pagan Test for Heteroskedasticity
# -----------------------------------------
# Ho: the variance is constant            
# Ha: the variance is not constant        
# 
# Data                  
# -------------------------------------
#   Response : sol_close 
# Variables: fitted values of sol_close 
# 
# Test Summary           
# -------------------------------
#   DF            =    1 
# Chi2          =    296.4985 
# Prob > Chi2   =    1.908327e-66 -> HETEROSCEDASTICITY


## plotting test ###############################################################

# adding fitted values e residuals to main dataframe
sol$yhat <- stepwised_model$fitted.values
sol$error <- stepwised_model$residuals



library("ggplot2")
library("tidyverse")

sol%>%
  ggplot()+
  geom_point(aes(x=yhat, y=error),
             color= "#55c667FF", size= 3) +
  labs(x= "stepwise model fitted values",
       y= "stepwise model residuals") +
  theme_bw()


## BOX-COX PRECEDURE ###########################################################
library("car")

#LAMBDA
lambda_bc <- powerTransform(sol$sol_close)

#adding almbda to DF
sol$bc_sol_close <- (((sol$sol_close ^ lambda_bc$lambda) - 1) / 
                       lambda_bc$lambda)

## new Box-cox model ###########################################################
model_bc <- lm(formula = bc_sol_close ~. -sol_close -yhat -error,
               data = sol)

## stepwise again  #############################################################
bc_setp_model <- step(object = model_bc,
                      k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))
summary(bc_setp_model)

## heteroscedasticity test ####################################################
ols_test_breusch_pagan(bc_setp_model)
ols_vif_tol(bc_setp_model)

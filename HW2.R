#############################################################################################
## 1.	Remove all records with NA entries. Find the number of the available records or rows ##
#############################################################################################
d = na.omit(airquality)
N = length(d$Ozone)
N

#############################################################################################
## 2.	Plot the dependent variable Ozone  as a function of the independent variable Solar.R ##
#############################################################################################
plot(d$Ozone~d$Solar.R, xlab = 'Solar.R', ylab = 'Ozone')

#################################################################################
## 3.	Evaluate the predictions using the following models for Ozone ~ Solar.R: ##
#################################################################################
#########################################################################################################
## 4.	In each case present the following: coefficient, summary statistics of the error vector, and SSE ##
################### Also include a plot that shows the response of these models. ########################
#########################################################################################################

## Eyeball Linear Equation ##

## One can tell that there is a positive slope by looking at the scatter plot ##
## Based on analysis. Guessing that intercept is at 15 and slope is 0.25 ##
p1 = 15 + 0.25*d$Solar.R
lines(d$Solar.R, p1, col = 2)
e1 = p1 - d$Ozone
summary(e1)
hist(e1)
SSE1 = sum(t(e1) * e1)
SSE1

## Linear Model ##
m2 = lm(d$Ozone~d$Solar.R)
c2 = coef(m2)
c2
p2 = c2[1] + c2[2]*d$Solar.R
lines(d$Solar.R, p2, col = 3)
e2 = p2 - d$Ozone
summary(e2)
hist(e2)
SSE2 = sum(t(e2) * e2)
SSE2

## Second order polynomial ##
x2 = d$Solar.R*d$Solar.R
m3 = lm(d$Ozone~d$Solar.R + x2)
c3 = coef(m3)
c3
p3 = c3[1] + c3[2]*d$Solar.R + c3[3]*x2
lines(d$Solar.R, p3, col = 4)
e3 = p3 - d$Ozone
summary(e3)
hist(e3)
SSE3 = sum(t(e3) * e3)
SSE3

## Generalized linear model ##
m4 = glm(d$Ozone~d$Solar.R, family = "poisson")
c4 = coef(m4)
c4
p4 = exp(c4[1] + c4[2]*d$Solar.R)
lines(d$Solar.R, p4, col = 6)
e4 = p4 - d$Ozone
summary(e4)
hist(e4)
SSE4 = sum(t(e4) * e4)
SSE4
# Question 1
# Consider the space shuttle data ?shuttle in the MASS library. 
# Consider modeling the use of the autolander as the outcome (variable name use). 
# Fit a logistic regression model with autoloader (variable auto) use (labeled as "auto" 1) 
# versus not (0) as predicted by wind sign (variable wind). 
# Give the estimated odds ratio for autoloader use comparing head winds, 
# labeled as "head" in the variable headwind (numerator) to tail winds (denominator).
library(MASS)
?shuttle
data(shuttle)
str(shuttle)
names(shuttle)
?glm
fit <- glm(use ~ wind, family='binomial', shuttle)
exp(fit$coeff)
# Answer: 0.969

# Question 2
# Consider the previous problem. Give the estimated odds ratio for autoloader use 
# comparing head winds (numerator) to tail winds (denominator) adjusting for 
# wind strength from the variable magn.
fit <- glm(use ~ wind + as.factor(magn), family='binomial', shuttle)
exp(fit$coeff)
# Answer: 0.969


# Question 3
# If you fit a logistic regression model to a binary variable, 
# for example use of the autolander, then fit a logistic regression 
# model for one minus the outcome (not using the autolander) what happens to the coefficients?
# Answer: The coefficients reverse their signs.

# Question 4
# Consider the insect spray data InsectSprays. 
# Fit a Poisson model using spray as a factor level. 
# Report the estimated relative rate comapring spray A (numerator) to spray B (denominator).
data(InsectSprays)
outp <- exp(coef(glm(count ~ as.factor(spray) - 1, family="poisson", InsectSprays)))
outp
outp[1]/outp[2]
# Answer: 0.9457

# Question 5
# Consider a Poisson glm with an offset, t. So, for example, 
# a model of the form glm(count ~ x + offset(t), family = poisson) where x 
# is a factor variable comparing a treatment (1) to a control (0) and t is 
# the natural log of a monitoring time. What is impact of the coefficient for 
# x if we fit the model glm(count ~ x + offset(t2), family = poisson) 
# where t2 <- log(10) + t? In other words, what happens to the coefficients 
# if we change the units of the offset variable. (Note, adding log(10) on the 
# log scale is multiplying by 10 on the original scale.)
# Answer: The coefficient estimate is unchanged
log(10)

# Question 6
# Using a knot point at 0, fit a linear model that looks like a hockey 
# stick with two lines meeting at x=0. Include an intercept term, x and 
# the knot point term. What is the estimated slope of the line after 0?
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
knots<-c(0)
splineTerms<-sapply(knots, function(knot)(x > knot)*(x - knot)) 
xMat<-cbind(1,x,splineTerms)
linearModel <- lm(y~xMat-1)
yhat<-predict(linearModel) 
plot(x,y,frame=FALSE,pch=21,bg="lightblue",cex=2) 
lines(x,yhat,col="red",lwd=2)
# Answer: 1.013


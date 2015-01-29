#1
data(mtcars)
fit <- lm(mpg ~ factor(cyl) + wt, data = mtcars)
summary(fit)
#2
data(mtcars)
fit1 <- lm(mpg ~ factor(cyl) + wt, data = mtcars)
fit2 <- lm(mpg ~ factor(cyl) , data = mtcars)
summary(fit1)
summary(fit2)
c(summary(fit1)$coef[3,1],  summary(fit2)$coef[3,1])


#3
data(mtcars)
fit1 <- lm(mpg ~ factor(cyl) + wt, data = mtcars)
fit2 <- lm(mpg ~ factor(cyl) *wt, data = mtcars)
# To compare model we usually use an anova table
# anova null hypothesis says that both models are the same.
compare <- anova(fit1, fit2)
compare$Pr
#4
lm(mpg ~ I(wt*0.5)+factor(cyl), data=mtcars)

#5
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
# Give the hat diagonal for the most influential point
fit <- lm(y ~ x)
hatvalues(fit)

#6
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
# Give the slope dfbeta for the point with the highest hat value.
fit <- lm(y ~ x)
dfbetas(fit)

#7
#it is possible to reverse the sign.
#both significant or non significant.
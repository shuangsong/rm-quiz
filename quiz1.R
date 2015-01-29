
#q1
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
w_bar<-mean(w)
z<-w*x/w_bar
mean(z)
#q2
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
fit.origin <- lm( y ~ x - 1 )
coef(fit.origin)
#q3

data(mtcars)
y<-mtcars$mpg
x<-mtcars$wt
fit<-lm(y ~ x,data=mtcars)
coef(fit)[2]

#q4
cor<-0.5
slope<-cor*2
slope
#q5
x<-1.5


#q6
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
x_bar<-mean(x)
sd.x<-sd(x)
(x[1]-x_bar)/sd.x
#q7
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

fit<-lm(y ~ x)
coef(fit)[1]
#q8
x<-c(-1, 1, 0)
y<-c( -4, 0, 4)
fit<-lm(y ~ x)
coef(fit)[1]
fit2<-lm(x ~ y)
coef(fit2)[1]

x<-c(-8, 8, 0)
y<-c( -7, 0, 7)
fit<-lm(y ~ x)
coef(fit)[1]
fit2<-lm(x ~ y)
coef(fit2)[1]


x<-c(0, 0, 0)
y<-c( 0, 0, 0)
fit<-lm(y ~ x)
coef(fit)[1]
fit2<-lm(x ~ y)
coef(fit2)[1]
#identically 0

#q9
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
miu<-mean(x)
miu

#10
sd(y)^2/sd(x)^2
var(y)/var(x)












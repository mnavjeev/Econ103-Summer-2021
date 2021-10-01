# Load the data 
data(mtcars)
############################################
attach(mtcars) 
# The above is generally not reccomended
# but it allows us to access variables 
# without having to reference the dataset

#Take a at the variables 
head(mtcars, 10)

# first scatter plot 
png(file="scatter1.png",width=1200, height=700, res=150) # To save the plot
plot(wt,mpg, main = "Weight vs. mpg for assorted cars", xlab = "Weight (in Tons)", ylab = "Miles per Gallon", col = "red")
dev.off()

# scatter plot with regression and y-bar lines 
png(file="scatter2.png",width=1200, height=700, res=150) # To save the plot 
plot(wt,mpg, main = "Weight vs. mpg for assorted cars", xlab = "Weight (in Tons)", ylab = "Miles per Gallon", col = "red")
abline(lm(mpg ~ wt), col = "blue")
abline(a= mean(mpg),  b= 0, col = "dark green")
dev.off()

# summarize regression of weight against mpg
summary(lm(mpg ~ wt))

# Lets create two different scatter plots by subsampling our data
sample1 = mtcars[sample(nrow(mtcars), 16), ]
sample2 = mtcars[sample(nrow(mtcars), 16), ]

png(file = "scatter3.png", width = 1200, height = 700, res = 150)
plot(sample1$wt, sample1$mpg, main = "Weight vs. mpg for assorted cars", xlab = "Weight (in Tons)", ylab = "Miles per Gallon", col = "red")
points(sample2$wt, sample2$mpg, col = "blue")
abline(lm(mpg ~ wt, data = sample1), col = "red")
abline(lm(mpg ~ wt, data = sample2), col = "blue")
legend(x = "topright", col = c("red", "blue"), legend = c("Sample 1", "Sample 2"), pch = 1)
dev.off()

# Summaries of the regressions from the two samples
summary(lm(mpg ~ wt, data = sample1))
summary(lm(mpg ~ wt, data = sample2))

# Variances are decreasing with sigma_\eps^2
## High Variances 
eps1 = rnorm(50, 0, 10)
eps2 = rnorm(50, 0, 10)
x1 = rnorm(50, 4, 10)
x2 = rnorm(50, 4, 10)
y1 = 1 + 0.5*x1 + eps1
y2 = 1 + 0.5*x2 + eps2

# Low Variances
leps1 = rnorm(50,0,1.5)
leps2 = rnorm(50,0,1.5)
lx1 = rnorm(50,4,10)
lx2 = rnorm(50,4,10	)
ly1 = 1 + 0.5*lx1 + leps1
ly2 = 1 + 0.5*lx2 + leps2

# Side by side plots
png(file = "low_v_high_1.png", width = 1200, height = 700, res = 150)
par(mfrow = c(1,2))
plot(x1, y1, col = "darkred", xlab = "X", ylab = "Y", main = "high error variance", pch = 16)
abline(lm(y1 ~ x1), col = "darkred")
plot(lx1, ly1, col = "darkred", xlab = "X", ylab = "Y", main = "low error variance", pch = 16) 
abline(lm(ly1 ~ lx1), col = "darkred")
dev.off()

# Side by side plots
png(file = "low_v_high_2.png", width = 1200, height = 700, res = 150)
par(mfrow = c(1,2))
plot(x1, y1, col = "darkred", xlab = "X", ylab = "Y", main = "high error variance", pch = 16)
points(x2, y2, col = "darkblue", pch = 16)
abline(lm(y1 ~ x1), col = "darkred")
abline(lm(y2 ~ x2), col = "darkblue")
plot(lx1, ly1, col = "darkred", xlab = "X", ylab = "Y", main = "low error variance", pch = 16) 
points(lx2, ly2, col = "darkblue", pch = 16)
abline(lm(ly1 ~ lx1), col = "darkred")
abline(lm(ly2 ~ lx2), col = "darkblue")
dev.off()

#Variances are decreasing with n
### High n 
eps1 = rnorm(50, 0, 1)
eps2 = rnorm(50, 0, 1)
x1 = runif(50)
x2 = runif(50)
y1 = 1 + 0.5*x1 + eps1
y2 = 1 + 0.5*x2 + eps2

# Low n
leps1 = rnorm(5, 0, 1)
leps2 = rnorm(5, 0, 1)
lx1 = runif(5)
lx2 = runif(5)
ly1 = 1 + 0.5*lx1 + leps1
ly2 = 1 + 0.5*lx2 + leps2

# Side by side plots
png(file = "low_v_high_n_1.png", width = 1200, height = 700, res = 150)
par(mfrow = c(1,2))
plot(x1, y1, col = "darkred", xlab = "X", ylab = "Y", main = "n=50", pch = 16, xlim = c(0,1))
abline(lm(y1 ~ x1), col = "darkred")
plot(lx1, ly1, col = "darkred", xlab = "X", ylab = "Y", main = "n=5", pch = 16, xlim = c(0,1)) 
abline(lm(ly1 ~ lx1), col = "darkred")
dev.off()

# Side by side plots
png(file = "low_v_high_n_2.png", width = 1200, height = 700, res = 150)
par(mfrow = c(1,2))
plot(x1, y1, col = "darkred", xlab = "X", ylab = "Y", main = "n=50", pch = 16, xlim = c(0,1))
points(x2, y2, col = "darkblue", pch = 16)
abline(lm(y1 ~ x1), col = "darkred")
abline(lm(y2 ~ x2), col = "darkblue")
plot(lx1, ly1, col = "darkred", xlab = "X", ylab = "Y", main = "n=5", pch = 16, xlim = c(0,1)) 
points(lx2, ly2, col = "darkblue", pch = 16)
abline(lm(ly1 ~ lx1), col = "darkred")
abline(lm(ly2 ~ lx2), col = "darkblue")
dev.off()


#Variances are decreasing with sigma_x
### High sigma_x
eps1 = rnorm(50, 0, 1)
eps2 = rnorm(50, 0, 1)
x1 = rnorm(50, 0, 1)
x2 = rnorm(50, 0, 1)
y1 = 1 + 0.5*x1 + eps1
y2 = 1 + 0.5*x2 + eps2

# Low sigma_x
leps1 = rnorm(50,0, 1)
leps2 = rnorm(50,0, 1)
lx1 = rnorm(50, 0, 0.075)
lx2 = rnorm(50, 0, 0.075)
ly1 = 1 + 0.5*lx1 + leps1
ly2 = 1 + 0.5*lx2 + leps2

# Side by side plots
png(file = "low_v_high_x_1.png", width = 1200, height = 700, res = 150)
par(mfrow = c(1,2))
plot(x1, y1, col = "darkred", xlab = "X", ylab = "Y", main = "high X variance", pch = 16, xlim = c(-2,2))
abline(lm(y1 ~ x1), col = "darkred")
plot(lx1, ly1, col = "darkred", xlab = "X", ylab = "Y", main = "low X variance", pch = 16, xlim = c(-2,2)) 
abline(lm(ly1 ~ lx1), col = "darkred")
dev.off()

# Side by side plots
png(file = "low_v_high_x_2.png", width = 1200, height = 700, res = 150)
par(mfrow = c(1,2))
plot(x1, y1, col = "darkred", xlab = "X", ylab = "Y", main = "high X variance", pch = 16, xlim = c(-2,2))
points(x2, y2, col = "darkblue", pch = 16)
abline(lm(y1 ~ x1), col = "darkred")
abline(lm(y2 ~ x2), col = "darkblue")

plot(lx1, ly1, col = "darkred", xlab = "X", ylab = "Y", main = "low X variance", pch = 16, xlim = c(-2,2)) 
points(lx2, ly2, col = "darkblue", pch = 16)
abline(lm(ly1 ~ lx1), col = "darkred")
abline(lm(ly2 ~ lx2), col = "darkblue")
dev.off()


# R^2 varies
eps1 = rnorm(50, 0, 10)
eps2 = rnorm(50, 0, 27)
x1 = runif(50, 5, 20)
x2 = runif(50, 5, 20)
lw1 = 5 + 5*x1 + eps1 
lw2 = 5 + 5*x2 + eps2
reg1 <- lm(lw1 ~ x1)
reg2 <- lm(lw2 ~ x2)
print(r1 <- summary(reg1)$r.squared)
print(r2 <- summary(reg2)$r.squared)
png(file = "R2.png", width = 1200, height = 700, res = 150)
par(mfrow = c(1,2))
plot(x1, lw1, col = "darkred", xlab = "Year of Education", ylab = "Log Wages", main = paste("R^2 = ", toString(round(r1,3))), pch = 16, xlim = c(5,20))
abline(reg1, col = "darkred")
plot(x2, lw2, col = "darkblue", xlab = "Years of Education", ylab = "Log Wages", main = paste("R^2 = ",toString(round(r2,3))), pch = 16, xlim = c(5,20)) 
abline(reg2, col = "darkblue")
dev.off()

# Nonlinear relationships
#eps = rnorm(100, 0, 3)
#x = rexp(100, 10)
y = 2 + 20*log(x) + eps
print(r2 <- summary(reg <- lm(y ~ x))$r.squared)
png(file = "nonlinear1.png", width = 1200, height = 700, res = 150)
par(mfrow = c(1,1))
plot(x, y, col = "darkred", xlab = "X", ylab = "Y", main = "E[Y|X=x] = 2 + 20*log(X)",pch = 16)
abline(reg, col = "darkred", lwd = 2)
dev.off()


library(pracma)
print(r3 <- summary(reg2 <- lm(y ~ I(log(x))))$r.squared)
xpred = linspace(min(x),min(y),1000)
ypred = reg2$coefficients[1] + reg2$coefficients[2]*log(xpred)

png(file = "nonlinear2.png", width = 1200, height = 700, res = 150)
par(mfrow = c(1,1))
plot(x, y, col = "darkred", xlab = "X", ylab = "Y", main = "E[Y|X=x] = 2 + 20*log(X)",pch = 16)
abline(reg, col = "darkred", lwd = 2)
lines(xpred,ypred, col = "darkblue", lwd = 2)
dev.off()

# Transformation of Y
y2 = exp(4 + 10*x) + 10*eps
logy2 = log(y2) 
print(r3 <- summary(reg3 <- lm(y2 ~ x))$r.squared)
print(r4 <- summary(reg4 <- lm(logy2 ~ x))$r.squared)
ypred2 = exp(reg4$coefficients[1] + reg4$coefficients[2]*xpred)


png(file = "nonlinearPoints2.png", width = 1200, height = 700, res = 150)
par(mfrow = c(1,1))
plot(x, y2, col = "darkred", xlab = "X", ylab = "Y", main = "E[Y|X=x] = ln(4 + 10x)",pch = 16)
dev.off()

png(file = "nonlinear3.png", width = 1200, height = 700, res = 150)
par(mfrow = c(1,1))
plot(x, y2, col = "darkred", xlab = "X", ylab = "Y", main = "E[Y|X=x] = ln(4 + 10x)",pch = 16)
abline(reg3, col = "darkred", lwd = 2)
dev.off()

png(file = "nonlinear4.png", width = 1200, height = 700, res = 150)
par(mfrow = c(1,1))
plot(x, y2, col = "darkred", xlab = "X", ylab = "Y", main = "E[Y|X=x] = ln(4 + 10x)",pch = 16)
abline(reg3, col = "darkred", lwd = 2)
lines(xpred,ypred2, col = "darkblue", lwd = 2)
dev.off()


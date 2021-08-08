#Generating some data to show overfitting.

eps = rnorm(30, 0, 0.9)
x = runif(30, 0, 5)
x.2 = x^2
x.3 = x^3
x.4 = x^4
x.5 = x^5
x.6 = x^6
x.7 = x^7
x.8 = x^8
x.9 = x^9
x.10 = x^10
y = 1 + x + eps

eps2 = rnorm(30, 0, 0.9)
x2 = runif(30, 0, 5)
y2 = 1 + x2 + eps2

summary(reg1<-lm(y ~ x))
summary(reg2 <- lm(y ~ x + x.2 + x.3 + x.4 + x.5 + x.6 + x.7 + x.8+x.9 + x.10))

library(pracma)
xpred = linspace(0,5,n=1000)
ypred1 = reg1$coefficients[1] + reg1$coefficients[2]*xpred
ypred2 = reg2$coefficients[1] + reg2$coefficients[2]*xpred + reg2$coefficients[3]*xpred^2 + reg2$coefficients[4]*xpred^3 + reg2$coefficients[5]*xpred^4 + reg2$coefficients[6]*xpred^5 + reg2$coefficients[7]*xpred^6 + reg2$coefficients[8]*xpred^7 + reg2$coefficients[9]*xpred^8 + reg2$coefficients[10]*xpred^9 + reg2$coefficients[11]*xpred^10

png(file = "overfit1_2.png", width = 1200, height = 700, res = 150)
par(mfrow = c(1,2))
plot(x, y, col = "darkred", xlab = "X", ylab = "Y", main = "Simple Linear Model", xlim = c(0,5),ylim = c(0,7),pch = 16)
lines(xpred,ypred1, col = "darkblue", lwd = 2)
plot(x,y, col = "darkred", xlab = "X", ylab = "Y", main = "10 Degree Polynomial Model", xlim = c(0,5), ylim = c(0,7), pch = 16) 
lines(xpred, ypred2, col = "darkblue", lwd = 2)
dev.off()

png(file = "overfit2_2.png", width = 1200, height = 700, res = 150)
par(mfrow = c(1,2))
plot(x2, y2, col = "darkgreen", xlab = "X", ylab = "Y", main = "Simple Linear Model",pch = 16, xlim = c(0,5), ylim = c(0,7))
lines(xpred,ypred1, col = "darkblue", lwd = 2)
plot(x2,y2, col = "darkgreen", xlab = "X", ylab = "Y", main = "10 Degree Polynomial Model", xlim = c(0,5), ylim = c(0,7), pch = 16) 
lines(xpred, ypred2, col = "darkblue", lwd = 2)
dev.off()

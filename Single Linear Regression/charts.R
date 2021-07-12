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



mean(sample1$mpg)

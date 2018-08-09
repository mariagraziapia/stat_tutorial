library(nortest)

# x at which data point is calculated
x<-c(-3., -1.5,  -1.2, -1, -0.5, 0, 0.5, 1.0, 1.2, 1.5, 3.0)
# Normal distribution with mean 0 and standard deviation 1
# y <- dnorm(x, mean=0, sd=1)
y <- c(0.00443, 0.12952, 0.19419, 0.24197, 0.35207, 0.39894, 
       0.35207, 0.24197, 0.19419, 0.12952, 0.00443)

# Plot normal distribution points
xPlot<-seq(-4, 4, length = 1000)
# dnorm gives the density for the normal distribution
yPlot<-dnorm(xPlot,mean=0,sd=1)
# pdf("normality1.pdf", width=15/2.54, height=12/2.54)
plot(xPlot, yPlot, xlab='x', ylab='y', type='l', 
     cex=0.5,
     cex.lab = 2, cex.axis = 2, 
     cex.main = 2, cex.sub = 2, lwd=4)
points(x,y,col='red',cex=1,lwd=8)
legend('topleft', title='Distribution', c('y'), fill=c('red'), cex=1, horiz=T)
# dev.off()

# Assess if data are normally distributed by using a hypothesis test
# Null hyphothesis: The data is normally distributed. 
# Let's set the significance level alpha = 0.05; therefore, if p<0.05, the hypothesis of normality is rejected

library(nortest)
pt<-pearson.test(y)
print(pt)

#Calculate p-values for different normality tests
y <- c(0.00443, 0.12952, 0.19419, 0.24197, 0.35207, 
       0.39894, 0.35207, 0.24197, 0.19419, 0.12952, 
       0.00443)

pt<-pearson.test(y) # Pearson chi-square test
cvmt<-cvm.test(y) # Cramer-von Mises test
adt<-ad.test(y) # Anderson-Darling test

# p-values
pvP<-pt$p.value
pvCvM<-cvmt$p.value
pvAD<-adt$p.value

message("---- Apply three different tests to assess the normality of a data distribution ----")
message("Normality test for y, Pearson chi-squared p-value")
print(pvP)
message("Normality test for y, Cramer-von Mises p-value")
print(pvCvM)
message("Normality test for y, Anderson-Darling p-value")
print(pvAD)

# Given two different data distributions, assess 
# if distributions are normally distributed by using hypothesis test
# y1 <- dnorm(x, mean=0, sd=1)
y1 <- c(0.00443, 0.12952, 0.19419, 0.24197, 0.35207, 
        0.39894, 0.35207, 0.24197, 0.19419, 0.12952, 
        0.00443)
# exponential distribution
# y2 <- dexp(x, 1/10)
y2 <- c(0.0, 0.0, 0.0, 0.0, 0.0, 0.10, 0.0951, 0.0905, 0.0887, 
        0.0861, 0.0741)

pt1<-cvm.test(y1)
pt2<-cvm.test(y2)

message("---- Apply Cramer-von Mises tests to assess the normality of two data distributions ----")
message("Normality test for y1, Cramer-von Mises p-value")
pv1<-pt1$p.value
print(pv1)
message("Normality test for y2, Cramer-von Mises p-value")
pv2<-pt2$p.value
print(pv2)

# Plot normal distribution points
# Plot exp distribution points
# pdf("normality2.pdf", width=15/2.54, height=12/2.54)
plot(xPlot, yPlot, xlab='x', ylab='y', type='l',
     cex=0.5,
     cex.lab = 2, cex.axis = 2, 
     cex.main = 2, cex.sub = 2, lwd=4)
y2Plot<-dexp(xPlot,1/10)
lines(xPlot,y2Plot,col='blue',cex=0.5,lwd=4)
points(x,y1,col='red',cex=1,lwd=8)
points(x,y2,col='green',cex=1,lwd=8)
legend('topleft', title='Distributions', c('y1','y2'), fill=c('red','green'), cex=1, horiz=T)
# dev.off()

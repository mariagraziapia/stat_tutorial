# Required package: randtests
library(randtests)

# x at which paired data are calculated
x<-c( -3., -1.5, -1, -0.5, 0, 0.501, 1.001, 1.501, 3.001)
# Gaussian 1: mean = 0, standard deviation = 1
g1<-dnorm(x, mean=0, sd=1)
# Gaussian 2: mean = 0, standard deviation = 5
g5<-dnorm(x, mean=0, sd=5)

# Wilcoxon signed rank test
wil<-wilcox.test(g1, g5, paired=TRUE)
print(wil)

# Difference between the paired gaussian values
gdiff<-(g5-g1)

# Difference sign test
difsig<-difference.sign.test(gdiff)
print(difsig)

dataList<-list(G1=g1, G5=g5)
kru<-kruskal.test(dataList)
print(kru)

# Kolmogorov-Smirnov goodness-of-fit test
ks<-ks.test(g1,g5)
print(ks)

# chi-squared
errors<-0.25*g5
piece<-(gdiff^2)/(errors^2)
chi2<-sum(piece)
pvchi2<-pchisq(chi2,length(piece),lower.tail=FALSE)
print(paste("chi2", pvchi2))

# Plot
# Gaussian lines
xPlot   <- seq(-5,5,length=1000)
g1Plot  <- dnorm(xPlot,mean=0, sd=1)
g5Plot<- dnorm(xPlot,mean=0, sd=5)
plot(xPlot,g1Plot, type="l", lwd=1, xlab="X", ylab="Y", las=1) 
lines(xPlot, g5Plot, col="red", lwd=2, lty=1)
# Points on the gaussians
points(x,g1)
points(x,g5, pch=15, col="red")
# Error bars
segments(x, g5+0.25*g5, x, g5-0.25*g5)

# pv<-c(wil$p.value, bar$p.value, difsig$p.value, kru$p.value, ks$p.value, pvchi2)
pv<-c(wil$p.value, difsig$p.value, kru$p.value, ks$p.value, pvchi2)
options(scipen=100)
print(pv)


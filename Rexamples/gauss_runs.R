# x at which paired data are calculated
x<-c( -3., -1.5,  -1.2, -1, -0.5, 0, 0.501, 1.001, 1.2001, 1.501, 3.001)
# Gaussian 1: mean = 0, standard deviation = 1
g1<-dnorm(x, mean=0, sd=1)
# Gaussian 2: mean = 0, standard deviation = 1
#g2<-dnorm(x, mean=0, sd=1)
coeff<-c(1.15, 1.05, 1.05, 1.05, 1.07, 1.05, 1.06, 1.04, 1.07, 0.97, 0.98)
#coeff<-c(1.02, 1.02, 1.02, 1.02, 1.02, 1.01, 1.01 , 1.01, 1.01)
g2<-g1*coeff
print(g2)

# Wilcoxon signed rank test
wil<-wilcox.test(g1, g2, paired=TRUE)
#print(wil)

# Difference between the paired gaussian values
gdiff<-(g2-g1)

# Bartels test
# bar<-bartels.rank.test(gdiff)
#print(bar)

# Difference sign test
difsig<-difference.sign.test(gdiff)
#print(difsig)

dataList<-list(G1=g1, G2=g2)
kru<-kruskal.test(dataList)
#print(kru)

ks<-ks.test(g1,g2)
print(ks)

# chi-squared
errors<-0.08*g2
piece<-(gdiff^2)/(errors^2)
chi2<-sum(piece)
pvchi2<-pchisq(chi2,length(piece),lower.tail=FALSE)
print(paste("chi2", pvchi2))


print(gdiff)
runsOut<-runs.test(gdiff, "two.sided", 0.)
print(runsOut)
pvRuns<-runsOut$p.value
print(pvRuns)


#p df("gauss_runs.pdf", width=15/2.54, height=12/2.54)
# Plot
# Gaussian lines
xPlot   <- seq(-5,5,length=1000)
g1Plot  <- dnorm(xPlot,mean=0, sd=1)
g2Plot<- dnorm(xPlot,mean=0, sd=1)
plot(xPlot, g1Plot, type="l", lwd=1, xlab="X", ylab="Y", las=1, ylim=c(0., max(g1+errors))) 
lines(xPlot, g1Plot, col="black", lwd=2, lty=1)
# Points on the gaussians
points(x,g1)
points(x,g2, pch=15, col="red")
# Error bars
segments(x, g2+errors, x, g2-errors)
# dev.off()

pv<-c(pvRuns, difsig$p.value, wil$p.value, kru$p.value, ks$p.value, pvchi2)
options(scipen=100)
names(pv)<-c("Runs", "Diffsign", "Wilcoxon signed rank", "Kruskal-Wallis", "KS", "chi2")
print(pv)

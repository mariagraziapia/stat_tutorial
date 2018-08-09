# Example showing that correlation is not the same as goodness-of-fit

data1 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
data2 <- c(0.1,	0.15,	0.25,	1.1,	1.5,	1.6,	1.65,	1.66,	1.67,	1.7)
xPlot <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

# Assume 10% error on data
err1 <- 0.1*data1
err2 <- 0.1*data2

# Pearson linear correlation coefficient
pearsonCorr<-cor(data1, data2,  method = "pearson", use = "complete.obs")
print(paste("Pearson correlation:", pearsonCorr))

# Kendall tau
kendallCorr<-cor(data1, data2,  method = "kendall", use = "complete.obs")
print(paste("Kendall tau:", kendallCorr))

# Spearmann rho
spearmanCorr<-cor(data1, data2,  method = "spearman", use = "complete.obs")
print(paste("Spearman rho:", spearmanCorr))

pdf("correl.pdf", width=15/2.54, height=12/2.54)
plot(xPlot, data1, type="l", tck=0.01, lwd=1, xlab="X", ylab="Y", las=1, ylim=c(0., max(data1+err1))) 
lines(xPlot, data1, col="black", lwd=2, lty=1)

points(xPlot, data1, pch=19, col="black")
points(xPlot, data2, pch=15, col="red")
# Error bars
segments(xPlot, data1+err1, x, data1-err1)
segments(xPlot, data2+err2, x, data2-err2)
dev.off()

# Chi-square test
piece<-((data1-data2)^2)/(err1^2 + err2^2)
chi2<-sum(piece)
pvchi2<-pchisq(chi2,length(piece),lower.tail=FALSE)
print(paste("chi2 p-value =", pvchi2))

# Kolmogorov-Smirnov test
ks<-ks.test(data1, data2)
print(ks)

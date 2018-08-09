# # Let us consider two measures
# x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
# y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)
# 
# # Assess their relationships
# # Get Pearson's cor coefficient
# cor(x, y)
# # Get Spearman's cor coefficient
# cor(x, y, method='spearman')
# # Get Kendall's cor coefficient
# cor(x, y, method='kendall')
# 
# 
# cor.test(x, y)
# cor.test(x, y, method='spearman')
# cor.test(x, y, method='kendall')


# Let us consider two measures and assess their relationship
x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)

# Calculate the correlation coefficient: Pearson (linear), Spearman rho, Kendall tau
pearson<-cor(x, y)
print(pearson)
rho<-cor(x, y, method='spearman')
print(rho)
tau<-cor(x, y, method='kendall')
print(tau)

# Test the significance of the correlation: here are the corresponding p-values
pvP<-cor.test(x, y)
pvS<-cor.test(x, y, method='spearman')
pvK<-cor.test(x, y, method='kendall')
print(pvP)
print(pvS)
print(pvK)

# Plot the data
# Abscissa: xPlot
xPlot<-c(1:length(x))
plot(xPlot, x, 
     ylim=c(min(y), max(x)),
     type='l',
     cex=0.5,
     xlab='',
     ylab='',
     cex.lab = 2, 
     cex.axis = 2, 
     cex.main = 2, 
     cex.sub = 2, 
     lwd=4)
lines(xPlot, y,
      cex=0.5,
      cex.lab = 2, 
      cex.axis = 2, 
      cex.main = 2, 
      cex.sub = 2, 
      lwd=4)
points(xPlot, x, col='red', cex=1, lwd=8)
points(xPlot, y, col='green', cex=1, lwd=8)
legend('topleft', 
       title='Measures',
       c('x','y'),
       fill=c('red','green'), 
       cex=1, 
       horiz=T)

# Goodness-of-fit test: Kolmogorov-Smirnov
ksTest<-ks.test(x,y)
print(ksTest)



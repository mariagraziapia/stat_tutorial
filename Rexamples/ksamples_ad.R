# Required package
library(kSamples)
# Conflict with ad.test in nortest package

# Define three data distributions
s1<-c(1.0066, -0.9587, 0.3462, -0.2653, -1.3872) 
s2<-c(0.1005, 0.2252, 0.4810, 0.6992, 1.9289) 
s3<-c(-0.7019, -0.4083, -0.9936, -0.5439, -0.3921) 

# Anderson-Darling k-sample test
adt<-ad.test(s1, s2, s3)
print(adt)

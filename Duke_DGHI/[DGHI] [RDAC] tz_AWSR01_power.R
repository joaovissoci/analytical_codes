
install.packages("clusterPower")
library(clusterPower)

#HTC
crtpwr.2prop(alpha = 0.05, 
			 power = 0.90, 
			 m = 7, 
			 n = NA, 
			 cv = 0.4, #0.65
			 p1 = 0.35, 
			 p2 = 0.20, 
			 icc = 0.03, #0.05 or #0.01
			 pooled = FALSE, 
			 p1inc = TRUE,
			 tol = .Machine$double.eps^0.25)


alpha The level of significance of the test, the probability of a Type I error.
power The power of the test, 1 minus the probability of a Type II error.
m The number of clusters per condition. It must be greater than 1.
n The mean of the cluster sizes.
cv The coefficient of variation of the cluster sizes. When cv = 0, the clusters all
have the same size.
p1 The expected proportion in the treatment group.
p2 The proportion in the control group.
icc The intraclass correlation.
pooled Logical indicating if pooled standard error should be used.
p1inc Logical indicating if p1 is expected to be greater than p2.
tol Numerical tolerance used in root finding. The default provides at least four
significant digits.

#contraceptives
crtpwr.2prop(alpha = 0.05, 
			 power = 0.90, 
			 m = 15, 
			 n = NA, 
			 cv = 0.65,
			 p1 = 0.23, 
			 p2 = 0.10, 
			 icc = 0.01, 
			 pooled = FALSE, 
			 p1inc = TRUE,
			 tol = .Machine$double.eps^0.25)

crtpwr.2prop(alpha = 0.05, 
			 power = 0.90, 
			 m = 15, 
			 n = NA, 
			 cv = 0.65,
			 p1 = 0.23, 
			 p2 = 0.10, 
			 icc = 0.05, 
			 pooled = FALSE, 
			 p1inc = TRUE,
			 tol = .Machine$double.eps^0.25)


#following METHOD 3 from https://academic.oup.com/ije/article/35/5/1292/762170
#cv<-((max(clustersize)-min(clustersize))/4)/mean(clustersize)

cv<-((100-20)/4)/mean(c(50,100,100,40,20,20,20))
cv
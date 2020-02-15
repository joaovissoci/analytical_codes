


#Site for a calculator: http://web1.sph.emory.edu/users/cdckms/samplesize%20icc%20deff2.html

library(epiR)

p <- 0.10 #expected prevalence
b <- 11 # units to be examined per cluster

D <- 1.5 # Design effect

rho <- (D - 1) / (b - 1) #ICC calculation = D = 


P = 0.35 #30% of injury
e = 0.05 #2% error margin
Z = 1.96 #which is to give us 95% confidence.
r = 0.05 #response rate loss of 5%

n=(Z^2*P*(P-1)/e^2)/1-r
n

epi.clustersize(p = 0.35, # expected prevalence
                b = 11, # sampling per school
                rho = 0.05, # difference among variance inside the clusters vs between clusters, i.e. caries prevalence inside every school expected to be homogeneous, and prevalence between schools expected to be homogenous also. 
                epsilon.r = 0.05/0.35, #the acceptable relative error. Relative error is error (MARGIN OF ERROR) / expected prevalence
                conf.level = 0.95)

epi.clustersize(p = 0.20, # expected prevalence
                b = 15, # sampling per school
                rho = 0.05, # difference among variance inside the clusters vs between clusters, i.e. caries prevalence inside every school expected to be homogeneous, and prevalence between schools expected to be homogenous also. 
                epsilon.r = 0.05/0.2, #the acceptable relative error. Relative error is error (MARGIN OF ERROR) / expected prevalence
                conf.level = 0.95)

epi.clustersize(p = 0.10, # expected prevalence
                b = 11, # sampling per school
                rho = 0.05, # difference among variance inside the clusters vs between clusters, i.e. caries prevalence inside every school expected to be homogeneous, and prevalence between schools expected to be homogenous also. 
                epsilon.r = 0.1/0.10, #the acceptable relative error. Relative error is error (MARGIN OF ERROR) / expected prevalence
                conf.level = 0.95)

(2 * 0.05x

N=(Z^2)*r*

(1.96^2)*(1-0.35)*1.8/0.65*5*(0.35*0.1)

#two-stage
cluster_ss_2 <- epi.clustersize(p = p, # expected prevalence
                b = b, # participants per cluster
                rho = 0.02, #ICC value
                epsilon.r = 0.1,
                conf.level = 0.95)


tn <- c(5, 3); tmean <- c(84, 28); tsigma2.x <- c(567, 160)

cluster_ss_3 <- epi.cluster2size(nbar = 2, 
								 n = tn, 
								 mean = tmean, 
								 sigma2.x = tsigma2.x,
								 sigma2.y = NA, 
								 sigma2.xy = NA, 
								 epsilon.r = 0.3,
								 method = "mean",
								 conf.level = 0.95)
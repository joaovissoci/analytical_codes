#########################################################################################
# Title: Geospatial Analysis of Unmet Cleft Care Need
# Investigators: Peter Bittar
# Principal Investigator: Alexander Allori, M.D.
# Statistician: Alfredo E. Farjat, Ph.D. (alfredo.farjat@duke.edu)
# Department: Surgery
# Date: 6/14/2017
# Last update: 6/12/2017

# In this code the raw data are analyzed
#########################################################################################

# Removes everything from memory
rm(list=ls())

# Libraries
library(gdata)
library(xtable)
library(ggplot2)
library(Hmisc)
library(Gmisc)

library(maptools)
library(maps)
library(spdep)
library(rgdal)
library(sp)
library(fields)
library(Matrix)
library(ctv)

library(spatialprobit)
library(geoR)
library(geoRglm)
library(spBayes)
library(scatterplot3d)

library(geosphere)

######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
# DATA (see data dictionary)

# Read raw data (original data set saved as .csv)
raw=read.csv(file='Data_spatial.csv')


######################################################################################################
######################################################################################################
######################################################################################################
# Data formating

# Patient Identifier
raw$Patient.Identifier<-factor(raw$Patient.Identifier)

# MRN
raw$MRN<-factor(raw$MRN)

# Date of first encounter
raw$First.Encounter<-as.Date(raw$First.Encounter, format='%Y-%m-%d')

# Date of last encounter
raw$Last.Encounter<-as.Date(raw$Last.Encounter, format='%Y-%m-%d')

# Actual Date of last encounter
raw$Actual.Last.Encounter<-as.Date(raw$Actual.Last.Encounter, format='%Y-%m-%d')

# Lost to follow-up
raw$Lost.to.Followup <- factor(raw$Lost.to.Followup, levels= c('No', 'Yes'))

# Status
# Phenotype
# Laterality
# Patient.Gender

# Patient Race
raw$Patient.Race<-factor(raw$Patient.Race, levels = c('White', 'Asian', 'Black', 'Other'))
levels(raw$Patient.Race)=c('White', 'Non-white', 'Non-white', 'Non-white')

# Date of birth
raw$Patient.Date.of.Birth<-as.Date(raw$Patient.Date.of.Birth, format='%Y-%m-%d')

# Patient.Street.Address
# Patient.City
# Patient.State

# Primary Postal Code
raw$Patient.Primary.Postal.Code<-factor(raw$Patient.Primary.Postal.Code)

# County
raw$Patient.County<-factor(raw$Patient.County)

# Latitude.of.USPS.Patient.Address
# Longitude.of.USPS.Patient.Address

# County FIPS codes
raw$County.FIPS.Code.of.USPS.Patient.Address<-factor(raw$County.FIPS.Code.of.USPS.Patient.Address)

# Tract FIPS Code
raw$Tract.FIPS.Code.of.USPS.Patient.Address<-factor(raw$Tract.FIPS.Code.of.USPS.Patient.Address)

# Block FIPS Code
raw$Block.FIPS.Code.of.USPS.Patient.Address<-factor(raw$Block.FIPS.Code.of.USPS.Patient.Address)

######################################################################################################
# Subset variables

vars<-c("Patient.Identifier",
        #  "MRN"
        "First.Encounter",                         
        "Last.Encounter", 
        "Actual.Last.Encounter", 
        "Lost.to.Followup", 
        "Status", 
        "Phenotype", 
        #  "Laterality", 
        "Patient.Gender", 
        "Patient.Race", 
        "Patient.Date.of.Birth", 
        #  "Patient.Street.Address", 
        #  "Patient.City", 
        "Patient.State", 
        #  "Patient.Primary.Postal.Code", 
        "Patient.County", 
        "Latitude.of.USPS.Patient.Address", 
        "Longitude.of.USPS.Patient.Address", 
        #  "County.FIPS.Code.of.USPS.Patient.Address", 
        #  "Tract.FIPS.Code.of.USPS.Patient.Address",
        #  "Block.FIPS.Code.of.USPS.Patient.Address", 
        #  "PCTPOPBLWPOV", 
        #  "PCTHHINCGT150K", 
        #  "MEDHHINC", 
        #  "PCTBACHDEG", 
        #  "PCTNOHSGRAD", 
        "SESINDEX", 
        #  "SESQUARTILE", 
        #  "Min.Dist.to.NC.Team", 
        #  "Dist.UNC", 
        #  "Dist.Duke", 
        #  "Dist.Wake.Forest", 
        "Rural")          

# Keep only variables in object vars
dat<-raw[,vars]

label(dat$Patient.Race)='Race'
label(dat$Patient.Gender)='Gender'
label(dat$SESINDEX)='Socio-economic Index'
label(dat$Rural)='Location type'


# Longitud of the treatment (years)
dat$Treatment.Length <- as.numeric( dat$Last.Encounter - dat$First.Encounter )/365
label(dat$Treatment.Length)='Treatment length (years)'

# Age at last encounter (years)
dat$Age.First.Encounter <- as.numeric(dat$First.Encounter - dat$Patient.Date.of.Birth)/365
label(dat$Age.First.Encounter)='Age at first encounter (years)'


# Age at last encounter (years)
dat$Age.Last.Encounter <- as.numeric(dat$Last.Encounter - dat$Patient.Date.of.Birth)/365
label(dat$Age.Last.Encounter)='Age at last encounter (years)'

# Lost to follow-up as 0s and 1s
dat$Y <-ifelse(dat$Lost.to.Followup=='Yes',1,0)


# Phenotype
label(dat$Phenotype)='Phenotype'
dat$Phenotype.CL<-ifelse(dat$Phenotype=='CL',1,0)
dat$Phenotype.CLA<-ifelse(dat$Phenotype=='CLA',1,0)
dat$Phenotype.CLP<-ifelse(dat$Phenotype=='CLP',1,0)
dat$Phenotype.CP<-ifelse(dat$Phenotype=='CP',1,0)


# Use observations only in neighboring states of NC
v<-as.character(dat$Patient.State) 
idx<-which(v=='NC' | v=='VA' | v=='WV' | v=='TN' | v=='GA' | v=='SC')

# Eliminate rows with missing values
dat<-na.omit(dat[idx,])
dat$Patient.State<-drop.levels(dat$Patient.State)


# Calculate distance to Duke
# Combine the spatial coordinates of patient address
s <- data.frame(LONG=dat$Longitude.of.USPS.Patient.Address, LAT=dat$Latitude.of.USPS.Patient.Address)

# Jitter duplicate coordinates to avoid problems fitting the spatial models
dc<-dup.coords(s)
idx<-as.numeric(c(dc[1,],dc[2,]))
s[idx,]<-jitter2d( s[idx,] ,max = 0.25)
dat$Longitude.of.USPS.Patient.Address<-s[,1]
dat$Latitude.of.USPS.Patient.Address<s[,2]

# Location projection
Sp<-mercator(s)/1000 

dat$LONG.p<-Sp[,1]
dat$LAT.p<-Sp[,2]

# Duke coordinates
Duke.Location<-matrix( c(-78.935934, 36.0043423), nrow=1 )

Duke.Loc.vec<-matrix( rep(Duke.Location,times=nrow(s)), ncol=2, byrow = T )

# Patients' Distance to Duke
dat$Dist.to.Duke<-rdist.earth.vec(x1=s,x2=Duke.Loc.vec,miles=FALSE)    
label(dat$Dist.to.Duke)='Distance to Duke (km)'



# Observations
X11(height = 10, width = 15)
par(mar=c(5,5,4,2))
plot(Duke.Location[,1], Duke.Location[,2], pch=19, cex=1.5,
     xlim = c(-86.5,-76),
     ylim = c(33, 39),
     main='Observations',
     cex.main=1.75,
     xlab='Longitude',
     ylab='Latitude',
     cex.lab=1.5)
map('state', add=T)
map('county','north carolina', add=T)
points( s[which(dat$Y==1),], pch=19, cex=1, col='red')
points( s[which(dat$Y==0),], pch=15, cex=1, col='darkgreen')
legend('topleft',
       legend = c('Yes','No'), 
       pch=c(19,15), 
       col=c('red','darkgreen'),
       cex=2,
       bty='n',
       title='Lost to follow-up')





######################################################################################################
#####################################################################################################
######################################################################################################
# Fit model ignoring spatial dependency

fit = glm(Y ~ 1 +
             Patient.Gender + 
             Patient.Race + 
             SESINDEX + 
             Age.First.Encounter +
             #Phenotype.CLA +
             #Phenotype.CLP +
             #Phenotype.CP,
             Phenotype +
             Dist.to.Duke + 
             Rural,
           family = 'binomial',
           data = dat)

summary(fit)





#####################################################################################################
######################################################################################################
#####################################################################################################
# Fit spatial model with all covariates (with geoRglm)

set.seed(4/3*pi^3)

# Create geodata object (data and covariates)
geo.dat<-as.geodata(dat, 
                    coords.col = c('LONG.p','LAT.p'), 
                    data.col = 'Y', 
                    covar.col = c("Patient.Gender", 
                                  "Patient.Race",
                                  "SESINDEX",
                                  "Age.First.Encounter", 
                                  "Phenotype.CL",
                                  "Phenotype.CLA",
                                  "Phenotype.CLP",
                                  "Phenotype.CP",
                                  "Dist.to.Duke",
                                  "Rural"))

# Covariate values at the data locations
trend.data<-trend.spatial(trend = ~ 1 + 
                            Patient.Gender + 
                            Patient.Race +
                            SESINDEX +
                            Age.First.Encounter +
                            Phenotype.CLA +
                            Phenotype.CLP +
                            Phenotype.CP + 
                            Dist.to.Duke +
                            Rural,
                          geodata=geo.dat)



# Model
model.par<-model.glm.control(trend.d = trend.data,
                             cov.model = 'matern',
                             kappa = 0.5) 


# Fit logistic regression to get MLE sensible starting values  
fit <- glm(Y ~ 1 +
             Patient.Gender + 
             Patient.Race +
             SESINDEX + 
             Age.First.Encounter + 
             Phenotype.CLA +
             Phenotype.CLP +
             Phenotype.CP +
             #Phenotype, # equivalent to fit only this
             Dist.to.Duke +
             Rural,
           family = binomial(link='logit'), 
           data = dat)



# Discretize values of phi (normalized exponential distribution)
phi.seq<-seq(from=0.01, to=20, length.out = 2001)
phi.values<-dexp(phi.seq, rate=1/4)
phi.values<-phi.values/sum(phi.values)


# Define prior distributions
prior.par<- prior.glm.control(beta.prior = 'normal',
                              beta = fit$coefficients, # coefficient from logistic regression
                              beta.var.std = summary(fit)$cov.unscaled, # variance from logistic regression
                              sigmasq.prior = "sc.inv.chisq",
                              sigmasq = 1, 
                              df.sigmasq =4,
                              phi.prior = phi.values,
                              #phi =  4, 
                              phi.discrete = phi.seq,
                              tausq.rel = 0)


# Define MCMC parameters 
mcmc.par <- mcmc.control(S.scale = 0.25, # proposal variance for updating S
                         S.start = 'default',
                         burn.in = 10000,
                         thin = 10,
                         n.iter = 510000, # number of iterations performed
                         phi.start = 1, # staring value for phi
                         phi.scale = 2) # proposal variance for the update of phi  


# Perform posterior simulation (by MCMC) and spatial prediction in 
# the binomial-logit spatial model.
test <- binom.krige.bayes(geodata = geo.dat,
                          model = model.par,
                          prior = prior.par, 
                          mcmc.input = mcmc.par) 




# Burn-in and number of samples after thinning 
n0=mcmc.par$burn.in/mcmc.par$thin
n.samples=mcmc.par$n.iter/mcmc.par$thin


# beta
for(i in 1:length(fit$coefficients)){
  beta.i<-paste('beta',i,sep = '.')
  assign(beta.i,test$posterior$beta$sample[i,])}

# sigmasq
sigmasq<-test$posterior$sigmasq$sample
chain.diagnostic(n0,n.samples,log(sigmasq),'log(sigma^2)')

# phi
phi<-test$posterior$phi$sample
chain.diagnostic(n0,n.samples,log(phi),'log(phi)')

# MCMC chains
chains<-cbind(beta.1, beta.2, beta.3, beta.4, beta.5,
              beta.6, beta.7, beta.8, beta.9, beta.10, 
              phi, sigmasq)

stats<-summary(mcmc(chains, start = n0+1, end = n.samples, thin=1))


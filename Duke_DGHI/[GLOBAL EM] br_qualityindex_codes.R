# install.packages("qgraph")
# install.packages("haven")
# install.packages("igraph")

library("haven")
library("qgraph")
library("igraph")
library("psych")

Lein_Data<-read.csv("/Users/joaovissoci/Desktop/AL_RawData.csv")

city_level_data<-read.csv("/Users/joaovissoci/Desktop/City.csv")

hc_level_data<-read.csv("/Users/jnv4/Desktop/HealthCenter (1).csv")




demographics<-read.csv("/Users/joaovissoci/Desktop/demograficos.csv",sep=";",dec=",")

colnames(demographics)<-c("IBGE",
                      "location",
                      "GDP",
                      "GINI",
                      "income",
                      "unemployment",
                      "literacy",
                      "population",
                      "urban_population",
                      "demographic_density",
                      "sus_coverage",
                      "procedures_secondary",
                      "procedures_tertiary",
                      "admissions_secondary",
                      "admissions_tertiary",
                      "elderly",
                      "infant_death",
                      "life_expectancy",
                      "sewage_access",
                      "garbage_access",
                      "water_access"
                      )

write.csv(demographics,"/Users/joaovissoci/Desktop/demograficos2.csv")

cor(na.omit(demographics[,-c(1,2)]))

data<-Lein_Data


# #MODEL 1 - PMAQ PCA
# model1_bea<-with(data,data.frame(
#   I_7_8_1,I_7_8_4, I_7_8_5, I_7_8_6, I_7_8_7, I_7_8_8, I_7_8_9, II_15_1, II_15_4, II_18_1, II_18_6, II_18_7, II_31_1_1, II_31_1_2, II_31_1_3, II_31_1_4, II_31_1_5, II_31_1_6, II_31_1_7, II_31_1_8, II_31_1_9, II_31_1_10, II_31_1_13, II_31_1_14))

# model1_bea<-na.omit(model1_bea)

# cor_data<-cor_auto(model1_bea)

# #Community analysis
# comprehension_network_glasso<-qgraph(cor_data,
#                                      layout="spring",
#                                      vsize=6,esize=20,graph="glasso",
#                                      sampleSize=nrow(model1_bea),
#                                      legend.cex = 0.5,GLratio=1.5,minimum=0.1)
# #Calculating Community measures
# g<-as.igraph(comprehension_network_glasso) #creating igraph object
# h<-walktrap.community(g) #creatin community object
# h<-spinglass.community(g, weights=NA)
# plot(h,g) #plotting community network
# h$membership #extracting community membership for each node on the network
# community<-data.frame(h$membership,rownames(cor_data))

# # par(mfrow=c(2,2)) #Command to configure the plot area for the scree plot graph
# # ev <- eigen(cor_data) # get eigenvalues - insert the data you want to calculate the scree plot for
# # ev # Show eigend values
# # ap <- parallel(subject=nrow(cor_data),var=ncol(cor_data),rep=100,cent=.05) #Calculate the acceleration factor
# # summary(ap)
# # nS <- nScree(ev$values) #Set up the Scree Plot 
# # # plotnScree(nS) # Plot the ScreePlot Graph
# # my.vss <- VSS(cor_data,title="VSS of BEA data")
# # #print(my.vss[,1:12],digits =2)
# # VSS.plot(my.vss, title="VSS of 24 mental tests")
# # scree(cor_data)
# # VSS.scree(cor_data)
# fa.parallel(cor_data,n.obs=229)

# # Pricipal Components Analysis
# # entering raw data and extracting PCs 
# # from the correlation matrix 
# fit <- psych::principal(cor_data,nfactors=3,rotate="promax",scores=TRUE)
# fit
# summary(fit) # print variance accounted for 
# loadings(fit) # pc loadings 
# #fit$scores
# pca1<-predict(fit,model1_bea)
# # scores<-scoreItems(fit$weights,bea_data[,-1],totals=TRUE)
# # summary(scores)
# # describe(scores$scores)
# # by(scores$scores,data_bea$risk_classification,summary)
# # wilcox.test(scores$scores[,1]~data_bea$risk_classification)
# # wilcox.test(scores$scores[,2]~data_bea$risk_classification)
# # wilcox.test(scores$scores[,3]~data_bea$risk_classification)
# # #wilcox.test(scores$scores[,4]~data_bea$risk_classification)

# model <- principal(cor_data ,nfactors=2, rotate='none', scores=T, cov=T)
# L <-fit$loadings            # Just get the loadings matrix
# S <- fit$scores              # This gives an incorrect answer in the current version

# d <- model1_bea              # get your data
# dc <- scale(d,scale=FALSE)     # center the data but do not standardize it
# pca1 <- dc %*% L                 # scores are the centered data times the loadings
# # lowerCor(sc)                   #These scores, being principal components
# #                                # should be orthogonal 


# # plot(model)
# qgraph.semModel(fit, manifest = NULL, layout = "spring", vsize.man = 3, 
#                 vsize.lat = 6, residuals = TRUE, latres = TRUE, curve = 0.2, residSize = 0.2,
#                 ...)


# #find PCA scores by city

# PCAMeans<-data.frame(pca1, zip=Lein_Data$ibge)
# library(plyr)
# r1<-ddply(PCAMeans, .(zip), summarize, mean=mean(RC1))
# names(PCAMeans)

# r2<-ddply(PCAMeans, .(zip), summarize, mean=mean(RC2))
# names(PCAMeans)

# r3<-ddply(PCAMeans, .(zip), summarize, mean=mean(RC3))
# names(PCAMeans)


# #regression
# Lein_Data$LNHACSC<-log(Lein_Data$hacsc)
# socio<-lm(LNHACSC~gdp+Income+Unemployment+GINIi+hdi, data=Lein_Data)

# system<-lm(LNHACSC~Investment+Coverage+PhysicianPercent+Physicians, data=Lein_Data)

# quality<-lm(LNHACSC~Comp1+Comp2+Comp3+Comp4, data=Lein_Data)

# summary(quality)

# #sociosystem
# adj1<-lm(LNHACSC~gdp+Income+Unemployment+GINIi+hdi+Investment+Coverage+PhysicianPercent+Physicians, data=Lein_Data)

# #socioquality

# #systemquality

# #sociosystemquality
# both<-lm(LNHACSC~Comp1+Comp2+Comp3+Comp4+gdp+Income+Unemployment+GINIi+hdi+Investment+Coverage+PhysicianPercent+Physicians, data=Lein_Data)

# Lein_Data$GDP<-Lein_Data$GDP/100000
# Lein_Data$Investment<-Lein_Data$Investment/100000
# Lein_Data$Income<-Lein_Data$Income/1000

sem_data<-with(hc_level_data,data.frame(
        Comp1=RC1,
        Comp2=RC2,
        Comp3=RC3,
        income,
        unemployment,
        Investment,
        Coverage,
        PhysicianPercent,
        HACSC=LNHACSC,
        literacy,
        urban_population,
        elderly,
        infant_death,
        gdp,
        gini
        # water_access
    ))

sem_data_scaled<-as.data.frame(scale(sem_data))

ses_data<-with(sem_data_scaled,data.frame(income,
                                          unemployment,
                                          literacy,
                                          urban_population,
                                          elderly,
                                          infant_death,
                                          gdp,
                                          gini
                                          ))

model <- principal(ses_data ,nfactors=1, rotate='oblimin', scores=T, cov=T)

# L <-fit$loadings            # Just get the loadings matrix
# S <- fit$scores              # This gives an incorrect answer in the current version

# d <- model1_bea              # get your data
# dc <- scale(d,scale=FALSE)     # center the data but do not standardize it
# pca1 <- dc %*% L                 # scores are the centered data times the loadings
# # lowerCor(sc)                   #These scores, being principal components
# #       

#Latent Variable Analysis
cfa_model <- '

#Latent variables         
# SES =~ literacy + urban_population + infant_death + gdp + income
# Ineq =~ gini + elderly + unemployment
HS =~ Coverage + Investment + PhysicianPercent
'
modelfit<-cfa(cfa_model, data=sem_data_scaled)
summary(modelfit, fit.measures=TRUE)
Est <- parameterEstimates(modelfit, ci = TRUE, standardized = TRUE)
subset(Est, op == "=~")
inspect(modelfit,"cov.lv")

library(lavaan)

model <- '

#Latent variables         
SES =~ urban_population + gdp + income
HS =~ Coverage + PhysicianPercent + Investment
# QAL =~ Comp1 + Comp2 + Comp3

#Regressions
HS ~ SES
Comp1 ~ HS
Comp2 ~ HS
Comp3 ~ HS
HACSC ~ HS + Comp1 + Comp2 + Comp3

#Residual Covariances
gdp ~~ gdp
# literacy ~~ literacy 
urban_population ~~ urban_population
# infant_death ~~ infant_death
income ~~ income
Coverage ~~ Coverage
Investment ~~ Investment
PhysicianPercent ~~ PhysicianPercent
Comp1 ~~ Comp1
Comp2 ~~ Comp2
Comp3 ~~ Comp3
HACSC ~~ HACSC
# GDP ~~ Income
# Comp1 ~~ Comp2
# Comp2 ~~ Comp3
# Comp3 ~~ Comp1
Coverage ~~  Investment
'

modelfit<-sem(model, data=sem_data_scaled) #fixed.x=FALSE)
# summary(modelfit)
summary(modelfit, fit.measures=TRUE)
lavaan::fitMeasures(modelfit, fit.measures = "all")
parameterEstimates(modelfit)
Est <- parameterEstimates(modelfit, ci = TRUE, standardized = TRUE)
subset(Est, op == "~")
subset(Est, op == "=~")

#network graph for SEM model

library(semPlot)
semPlot::semPaths(modelfit,layout="spring")

semPaths(modelfit,
        "std",
        layout="spring",
        residuals=FALSE,
        equalizeManifests=TRUE,
        edge.color="black",
        exoCov=FALSE,
        intercepts=FALSE,
        label.scale=FALSE,
        edge.label.cex=1,
        cut=0.4)
        # label.cex=labelcex,
        # color=color,
        # borders=borders, 
        # nodeLabels=nodeLabels)

### Modification Indexes
Mod <- modificationIndices(modelfit)
subset(Mod, mi > 10)

# #run 3 pca, one for each latent variable
# data<-Lein_Data
# model2_bea<-with(data,data.frame(Income, Unemployment, Investment, Coverage, PhysicianPercent, GDP, HDI, GINI, Comp1, Comp2, Comp3))
# cor_data<-cor_auto(model2_bea) 
# #Community analysis
# comprehension_network_glasso<-qgraph(cor_data,
#                                      layout="spring",
#                                      vsize=6,esize=20,graph="glasso",
#                                      sampleSize=nrow(model1_bea),
#                                      legend.cex = 0.5,GLratio=1.5,minimum=0.1)


# # Pricipal Components Analysis
# modelSE_bea<-with(data,data.frame(Income, Unemployment,GDP, HDI, GINI))
# modelHS_bea<-with(data,data.frame(Investment, Coverage, PhysicianPercent))
# modelQAL_bea<-with(data,data.frame(Comp1, Comp2, Comp3))

# cor_dataSE<-cor_auto(modelSE_bea)
# cor_dataHS<-cor_auto(modelHS_bea)
# cor_dataQAL<-cor_auto(modelQAL_bea)

# fitSE <- psych::principal(cor_dataSE, nfactors=1, rotate="promax",scores=TRUE)
# fit
# summary(fitSE) # print variance accounted for 
# loadings(fitSE) # pc loadings 

# fitHS <- psych::principal(cor_dataHS,nfactors=1,rotate="promax",scores=TRUE)
# fit
# summary(fitHS) # print variance accounted for 
# loadings(fitHS) # pc loadings 

# fitQAL <- psych::principal(cor_dataQAL,nfactors=1,rotate="promax",scores=TRUE)
# fit
# summary(fitQAL) # print variance accounted for 
# loadings(fitQAL) # pc loadings 


# #rescale
# Lein_Data$GDP<- scale(Lein_Data$GDP)
# Lein_Data$GINI<- scale(Lein_Data$GINI)
# Lein_Data$HDI<- scale(Lein_Data$HDI)
# Lein_Data$Income<- scale(Lein_Data$Income)
# Lein_Data$Unemployment<- scale(Lein_Data$Unemployment)

# #chart for community network analysis
# Lein_Data$PC<-Lein_Data$I_7_8_1
# Lein_Data$NC<-Lein_Data$I_7_8_4
# Lein_Data$PCI<-Lein_Data$I_7_8_5
# Lein_Data$OSP<-Lein_Data$I_7_8_6
# Lein_Data$VA<-Lein_Data$I_7_8_7
# Lein_Data$AS<-Lein_Data$I_7_8_8
# Lein_Data$ASD<-Lein_Data$I_7_8_9
# Lein_Data$PN<-Lein_Data$II_15_1
# Lein_Data$RL<-Lein_Data$II_15_4

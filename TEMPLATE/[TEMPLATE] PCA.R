##############################################################
#PCA Score comparisons
#############################################################
# # Define the amout of factor to retain
#Group of functinos to determine the number os items to be extracted
#par(mfrow=c(2,2)) #Command to configure the plot area for the scree plot graph
#ev <- eigen(cor_data) # get eigenvalues - insert the data you want to calculate the scree plot for
#ev # Show eigend values
#ap <- parallel(subject=nrow(cor_data),var=ncol(cor_data),rep=100,cent=.05) #Calculate the acceleration factor
#summary(ap)
#nS <- nScree(ev$values) #Set up the Scree Plot 
#plotnScree(nS) # Plot the ScreePlot Graph
#my.vss <- VSS(cor_data,title="VSS of BEA data")
#print(my.vss[,1:12],digits =2)
#VSS.plot(my.vss, title="VSS of 24 mental tests")
#scree(cor_data)
#VSS.scree(cor_data)
#fa.parallel(cor_data,n.obs=36)

library(psych)
# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 
fit <- principal(cor_data,4,rotate="varimax",scores=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
fit$scores
predict(fit,cor_data)
scores<-scoreItems(fit$weights,pca_data)
describe(scores$scores)
by(scores$scores,data_bea$risk_classification,summary)
wilcox.test(scores$scores[,1]~data_bea$risk_classification)
wilcox.test(scores$scores[,2]~data_bea$risk_classification)
wilcox.test(scores$scores[,3]~data_bea$risk_classification)
#wilcox.test(scores$scores[,4]~data_bea$risk_classification)
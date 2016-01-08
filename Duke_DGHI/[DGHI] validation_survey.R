#######################################################################################
#validation_survey.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
#######################################################################################
#this script follows a combination of the guidelines proposed by Hadley Wickham http://goo.gl/c04kq as well as using the formatR package http://goo.gl/ri6ky
#if this is the first time you are conducting an analysis using this protocol, please watch http://goo.gl/DajIN while following step by step

#link to manuscript

#####################################################################################
#SETTING ENVIRONMENT
#####################################################################################

#command below will install individual and is only run once. remove the hash tag if this is the first time you are running the code on RStudio, and then you can add the hash tag again
#install.packages("car", repos="http://cran.r-project.org")
#install.packages("ggplot2", repos="http://cran.r-project.org")

#command below will install each package. if you run this script from the beginning you need to run every single one again
lapply(c("epicalc", "sem","Hmisc","ggplot2", "psych","irr", "nortest", "moments","GPArotation","Kendall","nFactors","repmis","gdata","vcd","polycor","ROCR","pROC","vcd","psy"), library, character.only=T)

########################################################
#Datasets
########################################################

#if you are using a file that is local to your computer, then replace path below by path to the data file. command will throw all the data into the templateData object. replace the word template.data by a name that might easier for you to remember and that represents your data
data_cluster_police_rw <- read.csv("/Users/joaovissoci/Dropbox/datasets/DGHI/Africa_DGHI/surveyXpolice_validation/POL_RWA_severity.csv", sep=",")

data_cluster_survey_rw <- read.csv("/Users/joaovissoci/Dropbox/datasets/DGHI/Africa_DGHI/surveyXpolice_validation/SURVEY_RWA_severity.csv",sep=",")


#if you are using a file that is local to your computer, then replace path below by path to the data file. command will throw all the data into the templateData object. replace the word template.data by a name that might easier for you to remember and that represents your data
data_cluster_police_sl <- read.csv("/Users/joaovissoci/Dropbox/datasets/DGHI/Africa_DGHI/surveyXpolice_validation/police_SL_1612.csv",sep=",")

data_cluster_survey_sl <- read.csv("/Users/joaovissoci/Dropbox/datasets/DGHI/Africa_DGHI/surveyXpolice_validation/survey_sl_1612.csv",sep=",")


#if you are using a file that is local to your computer, then replace path below by path to the data file. command will throw all the data into the templateData object. replace the word template.data by a name that might easier for you to remember and that represents your data
data_cluster_police_rw <- read.csv("/home/joao/Dropbox/datasets/DGHI/Africa_DGHI/surveyXpolice_validation/POL_RWA_severity.csv", sep=",")

data_cluster_survey_rw <- read.csv("/home/joao/Dropbox/datasets/DGHI/Africa_DGHI/surveyXpolice_validation/SURVEY_RWA_severity.csv",sep=",")


#if you are using a file that is local to your computer, then replace path below by path to the data file. command will throw all the data into the templateData object. replace the word template.data by a name that might easier for you to remember and that represents your data
data_cluster_police_sl <- read.csv("/home/joao/Dropbox/datasets/DGHI/Africa_DGHI/surveyXpolice_validation/police_SL_1612.csv",sep=",")

data_cluster_survey_sl <- read.csv("/home/joao/Dropbox/datasets/DGHI/Africa_DGHI/surveyXpolice_validation/survey_sl_1612.csv",sep=",")

###

########################################################
#Agreemente Rwuanda data 
########################################################

#Recoding police data kernel density into low and high risk areas
##Adjusting NA's
data_cluster_police_rw$RISK<-data_cluster_police_rw$MEANpolice
data_cluster_police_rw$RISK<-car::recode(data_cluster_police_rw$RISK,"NA=0")
##Calculate tertiles
#quantile(data_cluster_police_rw$RISK, 0:4/4)
#data_cluster_police_rw$RISK<-car::recode(data_cluster_police_rw$RISK,"0.0000:1049.274=0;else=1")
data_cluster_police_rw$RISK<-car::recode(data_cluster_police_rw$RISK,"0.0000:349.758000=0;349.758001:1049.274=1;else=2")
#data_cluster_police_rw$RISK<-as.factor(data_cluster_police_rw$RISK)
#Recoding survey data kernel density into low and high risk areas
data_cluster_survey_rw$RISK<-data_cluster_survey_rw$MEANsurvey
data_cluster_survey_rw$RISK<-car::recode(data_cluster_survey_rw$RISK,"NA=0")
data_cluster_survey_rw$RISK<-car::recode(data_cluster_survey_rw$RISK,"0.0000:103.818400=0;103.818401:311.455200=1;else=2")
#data_cluster_survey_rw$RISK<-as.factor(data_cluster_survey_rw$RISK)

agree_data_rw<-data.frame(data_cluster_police_rw$RISK,data_cluster_survey_rw$RISK)
#agree_data_rw<-na.omit(agree_data_rw)
#agree_data_rw$data_cluster_police_rw.RISK<-car::recode(agree_data_rw$data_cluster_police_rw.RISK,"1=0;2=1;3=1")
#agree_data_rw$data_cluster_survey_rw.RISK<-car::recode(agree_data_rw$data_cluster_survey_rw.RISK,"1=0;2=1;3=1")
#Executing agreement nalysis
agree<-agree(agree_data_rw, tolerance=0) #% of Agreement
kappa<-cohen.kappa(agree_data_rw) #Kappa-value
AC1(kappa$agree)
#cor<-cor(agree_data_rw,method=c("kendall"))
#kendall<-Kendall(agree_data_rw$data_cluster_police_rw.RISK,agree_data_rw$data_cluster_survey_rw.RISK)
#poly<-hetcor(agree_data_rw)

obj1_rw<-ckappa(agree_data_rw)
rownames(obj1_rw$table)<-c("Low","Medium","High")
colnames(obj1_rw$table)<-c("Low","Medium","High")

agree_plot_rw<-agreementplot(obj1_rw$table, main = "Rwanda",xlab_rot=0, ylab_rot=90,xlab_just="center", ylab_just="center", xlab="Survey",ylab="Police")

agree_data_rw[,1]<-car::recode(agree_data_rw[,1],"0=0;else=1")
agree_data_rw[,2]<-car::recode(agree_data_rw[,2],"0=0;else=1")

agree_data_rw<-with(agree_data_rw,data.frame(as.numeric(data_cluster_police_rw.RISK),as.numeric(data_cluster_survey_rw.RISK)))

pred <- prediction(agree_data_rw[,1],agree_data_rw[,2])

## computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
#perf <- performance(pred,"tpr","fpr")
#plot(perf)
## precision/recall curve (x-axis: recall, y-axis: precision)
#perf1 <- performance(pred, "prec", "rec")
#plot(perf1)
## sensitivity/specificity curve (x-axis: specificity,
## y-axis: sensitivity)
perf1 <- performance(pred, "sens", "spec")
plot(perf1)

########################################################
#Agreemente SriLanka data
########################################################
#Recoding police data kernel density into low and high risk areas
##Adjusting NA's
data_cluster_police_sl$RISK<-data_cluster_police_sl$MEANpolice
data_cluster_police_sl$RISK<-car::recode(data_cluster_police_sl$RISK,"NA=0")
##Calculate tertiles
#quantile(data_cluster_police_sl$RISK, 0:4/4)
#data_cluster_police_sl$RISK<-car::recode(data_cluster_police_sl$RISK,"0.0000:1049.274=0;else=1")
data_cluster_police_sl$RISK<-car::recode(data_cluster_police_sl$RISK,"0.0000:67.823400=0;67.823401:203.470200=1;else=2")
#data_cluster_police_sl$RISK<-as.factor(data_cluster_police_sl$RISK)
#Recoding survey data kernel density into low and high risk areas
data_cluster_survey_sl$RISK<-data_cluster_survey_sl$MEANsurvey
data_cluster_survey_sl$RISK<-car::recode(data_cluster_survey_sl$RISK,"NA=0")
data_cluster_survey_sl$RISK<-car::recode(data_cluster_survey_sl$RISK,"0.0000:87.445400=0;87.445401:262.336200=1;else=2")
#data_cluster_survey_sl$RISK<-as.factor(data_cluster_survey_sl$RISK)

agree_data_sl<-data.frame(data_cluster_police_sl$RISK,data_cluster_survey_sl$RISK)
#agree_data_sl<-na.omit(agree_data_sl)
#agree_data_sl$data_cluster_police_sl.RISK<-car::recode(agree_data_sl$data_cluster_police_sl.RISK,"1=0;2=1;3=1")
#agree_data_sl$data_cluster_survey_sl.RISK<-car::recode(agree_data_sl$data_cluster_survey_sl.RISK,"1=0;2=1;3=1")
#Executing agreement nalysis
agree<-agree(agree_data_sl, tolerance=0) #% of Agreement
kappa<-cohen.kappa(agree_data_sl) #Kappa-value
AC1(kappa$agree)
#cor<-cor(agree_data_sl,method=c("kendall"))
#kendall<-Kendall(agree_data_sl$data_cluster_police_sl.RISK,agree_data_sl$data_cluster_survey_sl.RISK)
#poly<-hetcor(agree_data_sl)

obj1_sl<-ckappa(agree_data_sl)
rownames(obj1_sl$table)<-c("Low","Medium","High")
colnames(obj1_sl$table)<-c("Low","Medium","High")
agreementplot(obj1_sl$table, main = "Sri-Lanka",xlab_rot=0, ylab_rot=90,xlab_just="center", ylab_just="center", xlab="Survey",ylab="Police")

agree_data_sl[,1]<-car::recode(agree_data_sl[,1],"0=0;else=1")
agree_data_sl[,2]<-car::recode(agree_data_sl[,2],"0=0;else=1")

agree_data_sl<-with(agree_data_sl,data.frame(as.numeric(data_cluster_police_sl.RISK),as.numeric(data_cluster_survey_sl.RISK)))

pred <- prediction(agree_data_sl[,1],agree_data_sl[,2])

## computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
#perf <- performance(pred,"tpr","fpr")
#plot(perf)
## precision/recall curve (x-axis: recall, y-axis: precision)
#perf1 <- performance(pred, "prec", "rec")
#plot(perf1)
## sensitivity/specificity curve (x-axis: specificity,
## y-axis: sensitivity)
perf1 <- performance(pred, "sens", "spec")
plot(perf1)

########################################################
#ROC Plot with Sensitivity and Specificity
########################################################
#Initial ROC analysis
x<-roc(agree_data_rw[,2],agree_data_rw[,1]) #first argument = outcome; second = predictor
print(roc(agree_data_sl[,2],agree_data_sl[,1])) #first argument = outcome; second = predictor

#CI and Plotting
roc1 <- roc(agree_data_rw[,2],
            agree_data_rw[,1], percent=TRUE,
            # arguments for auc
            #partial.auc=c(100, 90), partial.auc.correct=TRUE,
            #partial.auc.focus="sens",
            # arguments for ci
            ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
            # arguments for plot
            plot=TRUE, auc.polygon=FALSE, max.auc.polygon=FALSE, grid=TRUE,
            print.auc=TRUE, show.thres=TRUE,col=c("black"),print.thres=T,print.auc.x=39, print.auc.y=60)
# Add to an existing plot. Beware of 'percent' specification!
roc2 <- roc(agree_data_sl[,2],
            agree_data_sl[,1],
            # arguments for ci
            ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
            # arguments for plot
            plot=TRUE, auc.polygon=FALSE, max.auc.polygon=FALSE, grid=TRUE,print.auc=TRUE,show.thres=TRUE,percent=TRUE,col=c("red"),add=TRUE,print.auc.x=59, print.auc.y=40,print.thres=T) 
legend("bottomright", legend=c("Rwanda", "Sri Lanka"),
            col=c(par("fg"), "red"), lwd=2)

sens.ci <- ci.se(roc1, specificities=seq(0, 100, 5))
plot(sens.ci, type="shape", col="lightgrey")
plot(sens.ci, type="bars",col="black")
plot(ci.thresholds(roc2))

#if you are using a file that is local to your computer, then replace path below by path to the data file. command will throw all the data into the templateData object. replace the word template.data by a name that might easier for you to remember and that represents your data
data_agreement2<-read.csv("/Users/joaovissoci/Desktop/data_validation.csv",
  header=T)

agree_data<-with(data_agreement2,data.frame(
  as.numeric(RISK_survey),as.numeric(RISK_police)))

#Executing agreement nalysis
agree<-agree(agree_data, tolerance=0) #% of Agreement
kappa<-cohen.kappa(agree_data) #Kappa-value 
cor<-cor(agree_data,method=c("spearman"))
kendall<-Kendall(agree_data[,1],agree_data[,2])
poly<-polychoric(agree_data)

##############################################################################
#PLOTS FOR THE PAPER
##############################################################################

tiff("/home/joao/Desktop/figure2A.tiff", units='in', width = 7, height = 6,compression = 'lzw',res=1200,bg = "white")
postscript("/home/joao/Desktop/figure2A.eps",width = 1200, height = 1200)
#par(mfrow=c(1,2))
agree_plot_rw<-agreementplot(obj1_rw$table, main = "Rwanda",xlab_rot=0, ylab_rot=90,xlab_just="center", ylab_just="center", xlab="Survey",ylab="Police")
dev.off()

tiff("/home/joao/Desktop/figure2B.tiff", units='in', width = 7, height = 6,compression = 'lzw',res=1200,bg = "white")
postscript("/home/joao/Desktop/figure2B.eps",width = 1200, height = 1200)
agree_plot_sl<-agreementplot(obj1_sl$table, main = "Sri-Lanka",xlab_rot=0, ylab_rot=90,xlab_just="center", ylab_just="center", xlab="Survey",ylab="Police")
dev.off()

#CI and Plotting
postscript("/home/joao/Desktop/figure3.eps",width = 1200, height = 1200)
tiff("/home/joao/Desktop/figure3.tiff", units='in',width = 5, height = 5,compression = 'lzw',res=1200)#,bg="white")
roc1 <- roc(agree_data_rw[,2],
            agree_data_rw[,1], percent=TRUE,
            # arguments for auc
            #partial.auc=c(100, 90), partial.auc.correct=TRUE,
            #partial.auc.focus="sens",
            # arguments for ci
            ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
            # arguments for plot
            plot=TRUE, auc.polygon=FALSE, max.auc.polygon=FALSE, grid=TRUE,
            print.auc=TRUE, show.thres=TRUE,col=c("black"),print.thres=T,print.auc.x=39, print.auc.y=60)
# Add to an existing plot. Beware of 'percent' specification!
roc2 <- roc(agree_data_sl[,2],
            agree_data_sl[,1],
            # arguments for ci
            ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
            # arguments for plot
            plot=TRUE, auc.polygon=FALSE, max.auc.polygon=FALSE, grid=TRUE,print.auc=TRUE,show.thres=TRUE,percent=TRUE,col=c("red"),add=TRUE,print.auc.x=59, print.auc.y=40,print.thres=T) 
legend("bottomright", legend=c("Rwanda", "Sri Lanka"),
            col=c(par("fg"), "red"), lwd=2)
dev.off()
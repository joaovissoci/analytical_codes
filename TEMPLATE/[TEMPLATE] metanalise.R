#######################################################################################
#example_metanalysis.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
#######################################################################################
#this script follows a combination of the guidelines proposed by Hadley Wickham http://goo.gl/c04kq as well as using the formatR package http://goo.gl/ri6ky
#if this is the first time you are conducting an analysis using this protocol, please watch http://goo.gl/DajIN while following step by step

#link to manuscript

#####################################################################################
#SETTING ENVIRONMENT
#####################################################################################
#command below will install individual and is only run once. remove the hash tag if this is the first time you are running the code on RStudio, and then you can add the hash tag again
install.packages("RCurl", repos="http://cran.r-project.org")
install.packages("metafor", repos="http://cran.r-project.org")
install.packages("meta", repos="http://cran.r-project.org")

#Load packages (after installed) with the library function
library(RCurl)
library(metafor)
library(meta)

########################################################################################################
#IMPORTING DATA AND RECODING
######################################################################################################
#Importing data set from the Spredsheet in google docs (Insert link)

##Call continuous data dataset
webdata1<- getURL(" "
,ssl.verifypeer = FALSE)
data_cont<-read.csv(textConnection(webdata1))

##Call binary data dataset
webdata2- getURL(" "
,ssl.verifypeer = FALSE)
data_bin<-read.csv(textConnection(webdata2))

##Call proportion data dataset
webdata3<- getURL(" "
,ssl.verifypeer = FALSE)
data_prop<-read.csv(textConnection(webdata3))

##Call correlation data dataset
webdata4<- getURL(" "
,ssl.verifypeer = FALSE)
data_cor<-read.csv(textConnection(webdata4))
###########################################################################################
#Example from Meta Package: Numeric Continuous data
###########################################################################################
## Example 1
meta1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, 
  data=data_cont, sm="SMD")
summary(meta2)
forest(meta1)
funnel(meta1)
metainf(meta1)
metainf(meta1, pooled="random")

## Example 2 with subgroup analysis
meta2 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, 
  data=data_cont, sm="SMD",byvar=group)
summary(meta2)
forest(meta2)
funnel(meta2)
metainf(meta2)
metainf(meta2, pooled="random")

###########################################################################################
#Example from Meta Package: Binary outcome 2x2
###########################################################################################
### Example 1
meta1 <- metabin(event.e, n.e, event.c, n.c,
data=data_bin, subset=c(41,47,51,59),
sm="RR", method="I")
summary(meta1)
funnel(meta1)
forest(meta1)
metainf(meta1)
metainf(meta1, pooled="random")

### Example 2 with subgroup analysis
meta2 <- metabin(event.e, n.e, event.c, n.c,
data=data_bin, subset=Olkin95$year<1970,
sm="RR", method="I",byvar=group)
summary(meta2)
forest(meta2)
funnel(meta2)
metainf(meta2)
metainf(meta2, pooled="random")

###########################################################################################
#Example from Meta Package: Single proportion (Prevalence)
###########################################################################################
##
meta1<- metaprop(casesTRUE, nTOTAL, sm="PLN",data=data_prop)
forest(meta1)
funnel(meta1)
metainf(meta1)
metainf(meta1, pooled="random")

##
meta2<- metaprop(casesTRUE, nTOTAL, sm="PLN", data=data_prop, byvar=group)
forest(meta2)
metainf(meta1)
metainf(meta1, pooled="random")
###########################################################################################
#Example from Meta Package: Correlation data
###########################################################################################
## Example 1
meta1 <- metacor(cor, n, data=data_cor,sm"COR")
summary(meta1)
forest(meta1)
funnel(meta1)
metainf(meta1)
metainf(meta1, pooled="random")

## Example 2 with subgroup analysis
meta2 <- metacor(cor, n, data=Fleiss93cont,byvar=group)
summary(meta2)
forest(meta2)
funnel(meta2)
metainf(meta2)
metainf(meta2, pooled="random")

###########################################################################################
#Example from Meta Package: Cumulative Metanalysis
###########################################################################################
##Calculating metanalysis object
meta1 <- metabin(event.e, n.e, event.c, n.c,
data=data_bin, studlab=author,
sm="RR", method="I")
summary(meta1)

## Calculating metanalysis cumulative model
metacum(meta1)
metacum(meta1, pooled="random")
forest(metacum(meta1, pooled="random"))

###########################################################################################
#Example from Meta Package: Metarregression
###########################################################################################
meta1 <- metacont(n.e, mean.e, sd.e,
n.c, mean.c, sd.c,
data=data_cont, sm="MD")
meta1<- update(meta1, byvar=group)
metareg(meta1)
metareg(meta1, ~group + age)
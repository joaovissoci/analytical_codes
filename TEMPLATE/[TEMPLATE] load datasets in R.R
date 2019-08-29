######################################################################
#LOADING DATA INTO R
######################################################################
#
#
# Ref: https://github.com/tidyverse/haven/blob/master/R/haven.R
#
#
######################################################################
#SETTING ENVIRONMENT
######################################################################

#laod packages
library(devtools)

# Or the the development version from GitHub:
# install.packages("devtools")
devtools::install_github("tidyverse/haven")

library(haven)
library(foreing)
library(readstata13)
######################################################################
#LOADING DATA FUNCTIONS
######################################################################

#CSV FORMAT

####### SAS
#haven package
read_sas("mtcars.sas7bdat")
write_sas(mtcars, "mtcars.sas7bdat")

#foreign package
mydata <- sasxport.get("c:/mydata.xpt")

####### SPSS
#haven package
read_sav("mtcars.sav")
write_sav(mtcars, "mtcars.sav")

#Hmisc package
mydata <- spss.get("c:/mydata.por", use.value.labels=TRUE)

####### Stata
#haven package
read_dta("mtcars.dta")
write_dta(mtcars, "mtcars.dta")

#foreign package
mydata <- read.dta("c:/mydata.dta")

#For stats 13 version
# install.packages("readstata13")

library(readstata13)
dat <- read.dta13("TEAdataSTATA.dta")


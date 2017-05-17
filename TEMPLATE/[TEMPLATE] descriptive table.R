# Test the script to build a descriptive table.
# Author: Elias Carvalho - ecacarva@gmail.com

# Clean environment
closeAllConnections()
rm(list=ls())

install.packages("/Users/joaovissoci/Desktop/bnpa_0.3.0.tar.gz")

# Load packages
library(bnpa)

# Set enviroment
# setwd("~/Documentos/R-Packages/scripts and data to test functions")

########## TESTING TABLEONE ########## Use dataToTestDesctitiveTable.csv
#  Load dataset - READ data_17_08_2016_V5.csv          # IF "" OR " " REPLACE WITH NA
data <- read.csv("/Users/joaovissoci/Desktop/dataToTestDesctitiveTable.csv") # Keep orginal data saved

# Load the data of work
data.to.work <- data
summary(data.to.work)

# Show a descritive table to all data
variables.to.table1 <- "all"
descritive.table(data.to.work, variables.to.table1)

#### Look at your work directory for a file names "table1.docx" ###

# Show a descritive table to all data
variables.to.table1 <- c("Age", "Gender","BMI","Education")
strata = "Gender"
descritive.table(data.to.work, variables.to.table1, strata)

#### Look at your work directory for a file names "table1.docx" ###


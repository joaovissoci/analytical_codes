# testBNPA.R - a script to test the execution of package
# Author: Elias Carvalho - ecacarva@gmail.com

# Clean environment
#closeAllConnections()
#rm(list=ls())

# Set enviroment
setwd("/Users/joaovissoci/Desktop")

# Load packages
library(bnpa)

# Show pre loaded data sets
head(dataQuantC)
head(dataQualiN)

#======= Example 1 ==============================================
# Load Continuous Data
data<-dataQuantC

# Generates a BN and PA information
bn.pa<-gera.bn.structure(data, bnhc=1)
# Add a white list (conections to add)
bn.pa<-gera.bn.structure(data, bnhc=1, wl="'C'-'E'")
# Add a black list (connections to exclude)
bn.pa<-gera.bn.structure(data, bnhc=1, wl="'C'-'E'", bl="'D'-'F'")

# Test MMHC algorithm  =========================================
bn.structure<-gera.bn.structure(data, bnmmhc=1)

# Test rsmax2 algorithm  =======================================
bn.structure<-gera.bn.structure(data, bnrsmax=1)

#======= Example 2 ==============================================
# Load Nominal Data
data<-dataQualiN

# Generates a BN and PA information
bn.pa<-gera.bn.structure(data, bnhc=1)
# Add a white list (conections to add)
bn.pa<-gera.bn.structure(data, bnhc=1, wl="'A'-'B'")
# Add a black list (connections to exclude)
bn.pa<-gera.bn.structure(data, bnhc=1, wl="'A'-'B'", bl="'E'-'T'")

# Test MMHC algorithm  =========================================
bn.structure<-gera.bn.structure(data, bnmmhc=1)

# Test rsmax2 algorithm  =======================================
bn.structure<-gera.bn.structure(data, bnrsmax=1)



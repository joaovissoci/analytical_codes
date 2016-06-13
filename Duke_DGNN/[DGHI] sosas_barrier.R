

install.packages("readstata13")
library(readstata13)
dat <- read.dta13("/Users/jnv4/Desktop/SOSAS_Ug_IndividualData_20151020.dta")

names(dat)

summary(dat$Prob1Reason_no_money)

data<-read.csv("/Users/joaovissoci/OneDrive - Duke University/datasets/DGNN/SOSAS/sosas_cleaned.csv")

barrier_data<-with(data,data.frame(
	Problem1_ReasonNoCare,
	Female,
	Gender,
	Age,
	Education,
	Literacy,
	Occupation,
	Ethnicity,
	Religion,
	Health_status
	))

## Questions
http://rfunction.com/archives/1499

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
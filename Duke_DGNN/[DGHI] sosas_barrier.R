
data<-read.csv("/Users/joaovissoci/OneDrive - Duke University/datasets/DGNN/SOSAS/sosas_cleaned.csv")

barrier_data<-with(data,data.frame(
	Female,
	Age,
	Education,
	Literacy,
	Occupation,
	Ethnicity,
	Religion,
	Health_status,
	O27_no_money,
	O27_no_transport,
	O27_no_time,
	O27_fear,
	O27_socialsupport,
	O27_no_need
	))

## Questions
* Reasons for care? Which variable it is?
* Q027 and Q020 ? What is the difference?
* Other confounders:
	* Traditional healer
	* Timing onset
	* Type of surgery capture.output
	* Seeking care?
	* which one is the weight to calculate proportions?
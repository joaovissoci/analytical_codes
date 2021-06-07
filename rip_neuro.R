
library(tidyr)
library(psych)

data<-read.csv("/Users/joaovissoci/Desktop/us_ripneuroall_data.csv")

#reshaping the data with tidyr (see http://www.milanor.net/blog/reshape-data-r-tidyr-vs-reshape2/)

data_tidy<-gather(data,
					key,
					value,
					-question_cat,
					-question,
					-category,
					-time)

data_tidy<-na.omit(data_tidy)

data_tidy$time_cat<-car::recode(data_tidy$time,"
										2015:2016=0;
										2017:2018=1")

data_tidy$group<-car::recode(data_tidy$key,"
										'duke_response_yes'=1;
										'duke_mean_score'=1;
										'nationa_response_yes'=0;
										'national_mean_score'=0")

#overall experience

data_overall_experience<-subset(data_tidy,data_tidy$question_cat=="q42")

#duke program summary
with(subset(data_overall_experience,
			data_overall_experience$key=="duke_response_yes"),
	 describeBy(value,time_cat))

#duke program summary
with(subset(data_overall_experience,
			data_overall_experience$key=="nationa_response_yes"),
	 describeBy(value,time_cat))

#DID model
didreg_overall= lm(value ~ group*time_cat, 
			data = data_overall_experience)

summary(didreg_overall)

#Q33 - Figure 4A

data_q33_experience_p1<-subset(data_tidy,
									  data_tidy$key=="duke_response_yes" |
									  data_tidy$key=="nationa_response_yes")

data_q33_experience<-subset(data_q33_experience_p1,
							data_q33_experience_p1$question_cat=="q33")

#duke program summary
with(subset(data_q33_experience,
			data_q33_experience$key=="duke_response_yes"),
	 describeBy(value,time_cat))

#duke program summary
with(subset(data_q33_experience,
			data_q33_experience$key=="nationa_response_yes"),
	 describeBy(value,time_cat))

#DID model
didreg_q33 = lm(value ~ group*time_cat, 
			data = data_q33_experience)

summary(didreg_q33)


#Q17 - Figure 4B

data_q17_experience_p1<-subset(data_tidy,
									  data_tidy$key=="duke_response_yes" |
									  data_tidy$key=="nationa_response_yes")

data_q17_experience<-subset(data_q17_experience_p1,
							data_q17_experience_p1$question_cat=="q17")

#duke program summary
with(subset(data_q17_experience,
			data_q17_experience$key=="duke_response_yes"),
	 describeBy(value,time_cat))

#duke program summary
with(subset(data_q17_experience,
			data_q17_experience$key=="nationa_response_yes"),
	 describeBy(value,time_cat))

#DID model
didreg_q17 = lm(value ~ group*time_cat, 
			data = data_q17_experience)

summary(didreg_q17)

#Q16 - Figure 4B

data_q16_experience_p1<-subset(data_tidy,
									  data_tidy$key=="duke_response_yes" |
									  data_tidy$key=="nationa_response_yes")

data_q16_experience<-subset(data_q16_experience_p1,
							data_q16_experience_p1$question_cat=="q16")

#duke program summary
with(subset(data_q16_experience,
			data_q16_experience$key=="duke_response_yes"),
	 describeBy(value,time_cat))

#duke program summary
with(subset(data_q16_experience,
			data_q16_experience$key=="nationa_response_yes"),
	 describeBy(value,time_cat))

#DID model
didreg_q16 = lm(value ~ group*time_cat, 
			data = data_q16_experience)

summary(didreg_q16)

#Q35 - Figure 4B

data_q35_experience_p1<-subset(data_tidy,
									  data_tidy$key=="duke_response_yes" |
									  data_tidy$key=="nationa_response_yes")

data_q35_experience<-subset(data_q35_experience_p1,
							data_q35_experience_p1$question_cat=="q35")

#duke program summary
with(subset(data_q35_experience,
			data_q35_experience$key=="duke_response_yes"),
	 describeBy(value,time_cat))

#duke program summary
with(subset(data_q35_experience,
			data_q35_experience$key=="nationa_response_yes"),
	 describeBy(value,time_cat))

#DID model
didreg_q35 = lm(value ~ group*time_cat, 
			data = data_q35_experience)

summary(didreg_q35)

#Average scores overall

data_overall_p1<-subset(data_tidy,
							data_tidy$key=="duke_response_yes" |
							data_tidy$key=="nationa_response_yes")

data_overall<-subset(data_overall_p1,
							data_overall_p1$question_cat!="q42")

didreg_all = lm(value ~ group*time_cat, 
			data = data_overall)

summary(didreg_all)


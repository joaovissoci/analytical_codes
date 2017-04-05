
load("/Users/jnv4/Desktop/zika_completo.RData")

library("lubridate")

testing_model<-subset(apendectomy, ANO_CMPT==2015|
								   ANO_CMPT==2016)

testing_model$date<-with(
	testing_model, paste(ANO_CMPT,MES_CMPT,
		01,sep="/"))

testing_model$date<-ymd(testing_model$date)

#ICD codes
#### Abortion - O01 a O08
# "O060"
# "O061" "O062" "O063" "O064" 
# "O065" "O066" 
# "O067" "O068" 
# "O069" 

testing_model_O001<-subset(testing_model,
						  DIAG_PRINC=="O060" |
						  DIAG_PRINC=="O061" |
						  DIAG_PRINC=="O062" |
						  DIAG_PRINC=="O063" |
						  DIAG_PRINC=="O064" |
						  DIAG_PRINC=="O065" |
						  DIAG_PRINC=="O066" |
						  DIAG_PRINC=="O067" |
						  DIAG_PRINC=="O068" |
						  DIAG_PRINC=="O069"
						  )

testing_model_O001$case<-1
with(testing_model_O001,by(case,ANO_CMPT,sum))

# date_month <- floor_date(
# 	testing_model_O001$MES_CMPT, 
# 	"month")

time_series_month_O01<-ddply(testing_model_O001,
	"date", summarise, 
	cases_month = sum(case))

#plotting time series by month
ggplot(time_series_month_O01,aes(date,
							 cases_month)) + 
  geom_line() +
#  geom_line(data=data2,aes(color="Speeding")) +
#  labs(color="Legend") +
#  scale_colour_manual("", breaks = c("Distracted Driving", "Speeding"),
#                          values = c("blue", "brown")) +
  #ggtitle("Closing Stock Prices: IBM & Linkedin") + 
  #scale_x_date(format = "%b-%Y") +
  theme(plot.title = element_text(lineheight=.7, 
  	face="bold")) +
  theme_bw()

#### Contraception - z300, z301, z304, z308 e z309
# "Z300" "Z301" "Z304" "Z308" "Z309"

testing_model_Z300<-subset(testing_model,
						  DIAG_PRINC=="Z300" |
						  DIAG_PRINC=="Z301" |
						  DIAG_PRINC=="Z304" |
						  DIAG_PRINC=="Z308" |
						  DIAG_PRINC=="Z309"
						  )

testing_model_Z300$case<-1
with(testing_model_Z300,by(case,ANO_CMPT,sum))

# date_month <- floor_date(
# 	testing_model_Z300$MES_CMPT, 
# 	"month")

time_series_month_Z300<-ddply(testing_model_Z300,
	"date", summarise, 
	crashes_month = sum(case))

#plotting time series by month
ggplot(time_series_month_Z300,aes(date,
							 crashes_month)) + 
  geom_line() +
#  geom_line(data=data2,aes(color="Speeding")) +
#  labs(color="Legend") +
#  scale_colour_manual("", breaks = c("Distracted Driving", "Speeding"),
#                          values = c("blue", "brown")) +
  #ggtitle("Closing Stock Prices: IBM & Linkedin") + 
  #scale_x_date(format = "%b-%Y") +
  theme(plot.title = element_text(lineheight=.7, 
  	face="bold"))+
  theme_bw()

#### Procreation - z31
# "Z310" "Z312"

testing_model_Z310<-subset(testing_model,
						  DIAG_PRINC=="Z310" |
						  DIAG_PRINC=="Z312" |
						  DIAG_PRINC=="Z319"
						  )

testing_model_Z310$case<-1
with(testing_model_Z310,by(case,ANO_CMPT,sum))

# date_month <- floor_date(
# 	testing_model_Z310$MES_CMPT, 
# 	"month")

time_series_month_Z310<-ddply(testing_model_Z310,
	"date", summarise, 
	crashes_month = sum(case))

#plotting time series by month
ggplot(time_series_month_Z310,aes(date,
							 crashes_month)) + 
  geom_line() +
#  geom_line(data=data2,aes(color="Speeding")) +
#  labs(color="Legend") +
#  scale_colour_manual("", breaks = c("Distracted Driving", "Speeding"),
#                          values = c("blue", "brown")) +
  #ggtitle("Closing Stock Prices: IBM & Linkedin") + 
  #scale_x_date(format = "%b-%Y") +
  theme(plot.title = element_text(lineheight=.7, 
  	face="bold"))+
  theme_bw()

#### Sterilization - z302
# "Z302" "Z319"

testing_model_Z302<-subset(testing_model,
						  DIAG_PRINC=="Z302")						  )

testing_model_Z302$case<-1
with(testing_model_Z302,by(case,ANO_CMPT,sum))

# date_month <- floor_date(
# 	testing_model_Z302$MES_CMPT, 
# 	"month")

time_series_month_Z302<-ddply(testing_model_Z302,
	"date", summarise, 
	crashes_month = sum(case))

#plotting time series by month
ggplot(time_series_month_Z302,aes(date,
							 crashes_month)) + 
  geom_line() +
#  geom_line(data=data2,aes(color="Speeding")) +
#  labs(color="Legend") +
#  scale_colour_manual("", breaks = c("Distracted Driving", "Speeding"),
#                          values = c("blue", "brown")) +
  #ggtitle("Closing Stock Prices: IBM & Linkedin") + 
  #scale_x_date(format = "%b-%Y") +
  theme(plot.title = element_text(lineheight=.7, 
  	face="bold"))+
  theme_bw()

#### Pragnancy termination - z332
 
# testing_model_Z332<-subset(testing_model,
# 						  DIAG_PRINC=="Z302" |
# 						  DIAG_PRINC=="Z319"
# 						  )

# testing_model_Z332$case<-1

# # date_month <- floor_date(
# # 	testing_model_Z332$MES_CMPT, 
# # 	"month")

# time_series_month_Z302<-ddply(testing_model_Z332,
# 	"MES_CMPT", summarise, 
# 	crashes_month = sum(case))

# #plotting time series by month
# ggplot(time_series_month_Z302,aes(MES_CMPT,
# 							 crashes_month)) + 
#   geom_line() +
# #  geom_line(data=data2,aes(color="Speeding")) +
# #  labs(color="Legend") +
# #  scale_colour_manual("", breaks = c("Distracted Driving", "Speeding"),
# #                          values = c("blue", "brown")) +
#   #ggtitle("Closing Stock Prices: IBM & Linkedin") + 
#   #scale_x_date(format = "%b-%Y") +
#   theme(plot.title = element_text(lineheight=.7, 
#   	face="bold"))

#### Vaginally delivery - O80
# "O800" "O801" "O808" "O809" 

testing_model_O80<-subset(testing_model,
						  DIAG_PRINC=="O800" |
						  DIAG_PRINC=="O801" |
						  DIAG_PRINC=="O808" |
						  DIAG_PRINC=="O809")

testing_model_O80$case<-1
with(testing_model_O80,by(case,ANO_CMPT,sum))

# date_month <- floor_date(
# 	testing_model_O80$MES_CMPT, 
# 	"month")

time_series_month_O80<-ddply(testing_model_O80,
	"date", summarise, 
	crashes_month = sum(case))

#plotting time series by month
ggplot(time_series_month_O80,aes(date,
							 crashes_month)) + 
  geom_line() +
#  geom_line(data=data2,aes(color="Speeding")) +
#  labs(color="Legend") +
#  scale_colour_manual("", breaks = c("Distracted Driving", "Speeding"),
#                          values = c("blue", "brown")) +
  #ggtitle("Closing Stock Prices: IBM & Linkedin") + 
  #scale_x_date(format = "%b-%Y") +
  theme(plot.title = element_text(lineheight=.7, 
  	face="bold"))+
  theme_bw()


#### C-section - O82
# "O820"
# "O821" "O822" "O828" "O829"

testing_model_O820<-subset(testing_model,
						  DIAG_PRINC=="O820" |
						  DIAG_PRINC=="O821" |
						  DIAG_PRINC=="O822" |
						  DIAG_PRINC=="O828" |
						  DIAG_PRINC=="O829")

testing_model_O820$case<-1
with(testing_model_O820,by(case,ANO_CMPT,sum))

# date_month <- floor_date(
# 	testing_model_O820$MES_CMPT, 
# 	"month")

time_series_month_O820<-ddply(testing_model_O820,
	"date", summarise, 
	cases_month = sum(case))

#plotting time series by month
ggplot(time_series_month_O820,aes(date,
							 cases_month)) + 
  geom_line() +
#  geom_line(data=data2,aes(color="Speeding")) +
#  labs(color="Legend") +
#  scale_colour_manual("", breaks = c("Distracted Driving", "Speeding"),
#                          values = c("blue", "brown")) +
  #ggtitle("Closing Stock Prices: IBM & Linkedin") + 
  #scale_x_date(format = "%b-%Y") +
  theme(plot.title = element_text(lineheight=.7, 
  	face="bold"))+
  theme_bw()

#### Preterm labor - O68
# "O680" "O681" "O682" "O683" "O688" "O689"

testing_model_O68<-subset(testing_model,
						  DIAG_PRINC=="O680" |
						  DIAG_PRINC=="O681" |
						  DIAG_PRINC=="O682" |
						  DIAG_PRINC=="O683" |
						  DIAG_PRINC=="O688" |
						  DIAG_PRINC=="O689")

testing_model_O68$case<-1
with(testing_model_O68,by(case,ANO_CMPT,sum))

# date_month <- floor_date(
# 	testing_model_O68$MES_CMPT, 
# 	"month")

time_series_month_O68<-ddply(testing_model_O68,
	"date", summarise, 
	crashes_month = sum(case))

#plotting time series by month
ggplot(time_series_month_O68,aes(date,
							 crashes_month)) + 
  geom_line() +
#  geom_line(data=data2,aes(color="Speeding")) +
#  labs(color="Legend") +
#  scale_colour_manual("", breaks = c("Distracted Driving", "Speeding"),
#                          values = c("blue", "brown")) +
  #ggtitle("Closing Stock Prices: IBM & Linkedin") + 
  #scale_x_date(format = "%b-%Y") +
  theme(plot.title = element_text(lineheight=.7, 
  	face="bold")) +
  theme_bw()


#### SDs - A50
#"A500" "A501" "A502" "A503" "A504" "A505" 
# "A506" "A507" "A509"
 
testing_model_A500<-subset(testing_model,
						  DIAG_PRINC=="A500" |
						  DIAG_PRINC=="A501" |
						  DIAG_PRINC=="A502" |
						  DIAG_PRINC=="A503" |
						  DIAG_PRINC=="A504" |
						  DIAG_PRINC=="A505" |
						  DIAG_PRINC=="A506" |
						  DIAG_PRINC=="A507" |
						  DIAG_PRINC=="A509")

testing_model_A500$case<-1
with(testing_model_A500,by(case,ANO_CMPT,sum))

# date_month <- floor_date(
# 	testing_model_A500$MES_CMPT, 
# 	"month")

time_series_month_A500<-ddply(testing_model_A500,
	"date", summarise, 
	crashes_month = sum(case))

#plotting time series by month
ggplot(time_series_month_A500,aes(date,
							 crashes_month)) + 
  geom_line() +
#  geom_line(data=data2,aes(color="Speeding")) +
#  labs(color="Legend") +
#  scale_colour_manual("", breaks = c("Distracted Driving", "Speeding"),
#                          values = c("blue", "brown")) +
  #ggtitle("Closing Stock Prices: IBM & Linkedin") + 
  #scale_x_date(format = "%b-%Y") +
  theme(plot.title = element_text(lineheight=.7, 
  	face="bold")) +
  theme_bw()









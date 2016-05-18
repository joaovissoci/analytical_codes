#################################################################
# SUICIDE PREVENTION INITIATIVES SYSTEMATIC REVIEW
#################################################################
#
#
#
#
#
#################################################################
#SETTING ENVIRONMENT
#################################################################
#All packages must be installes with install.packages() function
lapply(c(""), 
library, character.only=T)
#################################################################
#IMPORTING DATA
#################################################################
#LOADING DATA FROM A .CSV FILE
data<-read.csv("/Users/joaovissoci/OneDrive - Duke University/datasets/DGHI/GIC 2016/suicide_SR.csv",sep=",")
#information between " " are the path to the directory in your computer where the data is stored

#############################################################################
#DATA MANAGEMENT
#############################################################################

#############################################################################
#CONTINUOUS DATA METANALYSIS MODEL
#############################################################################
## Example 1
meta_model<-with(data,data.frame(study,outcome,metaanalysis,
	mean_FUP1_experimental_group,
	sd_FUP1_experimental_group,
	mean_FUP1_experimental_control,
	sd_FUP1_experimental_control,
	N_group_FUP1,
	N_control_FUP1))
meta1 <- metacont(N_group_FUP1, 
	mean_FUP1_experimental_group,
	sd_FUP1_experimental_group,
	N_control_FUP1,
	mean_FUP1_experimental_control,
	sd_FUP1_experimental_control, 
  data=meta_model, sm="SMD")
summary(meta2)
forest(meta1)
funnel(meta1)
metainf(meta1)
metainf(meta1, pooled="random")

#############################################################################
#DICHOTOMOUS DATA METANALYSIS MODEL
#############################################################################
### Example 1
meta1 <- metabin(event.e, n.e, event.c, n.c,
data=data_bin, subset=c(41,47,51,59),
sm="RR", method="I")
summary(meta1)
funnel(meta1)
forest(meta1)
metainf(meta1)
metainf(meta1, pooled="random")

##############################################################################
#END
##############################################################################
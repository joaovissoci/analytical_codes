#################################################################
#IHD GEO EPI - BRAZIL
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
lapply(c("mlmRev","lme4"), 
library, character.only=T)
#################################################################
#IMPORTING DATA
#################################################################
#LOADING DATA FROM A .CSV FILE
data<-read.csv("/Users/joaovissoci/OneDrive - Duke University/datasets/DGHI/Brazil/epidemiologic transition/ihd_cities.csv",
	sep=",")

#############################################################################
#DATA MANAGEMENT
#############################################################################

regdata<-with(data,data.frame(Regio,SMR_IHD,Distance,
	distance75))

#############################################################################
#MULTILEVEL MODELING
#############################################################################

model1<-lmer(SMR_IHD ~ Regio * Distance + (1 + Distance | Regio), 
	data=regdata)

summary(model1) 
#intervals(m1.lme4)
anova(model1)
#generating coefficients of whatever
coefs <- data.frame(coef(summary(model1)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
# get Satterthwaite-approximated degrees of freedom
coefs$df.Satt <- coef(summary(model1))[, 3]
# get approximate p-values
coefs$p.Satt <- coef(summary(model1))[, 5]
coefs
difflsmeans(model1)
plot(difflsmeans(model1, test.effs="groups:moments"))
lsmeans(model1)
confint(model1,level=0.95)
lmmpower(model1, pct.change = 0.30, t = seq(0,5,1), power = 0.80)

model1_2 = lmer(outcome ~ groups + moments + (1|subject),data = nlmedata,REML=FALSE)

model1_3 = lmer(outcome ~ groups + (1|subject),data = nlmedata,REML=FALSE)

anova(model1,model1_2,model1_3)

lmmpower(model1, pct.change = 0.40, t = seq(0,3,1), power = 0.80)


##############################################################################
#END
##############################################################################
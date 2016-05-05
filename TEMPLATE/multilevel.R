############################################################
# TEMPLATE MULTILEVEL MODELING
############################################################

# Resources
#http://www.rensenieuwenhuis.nl/r-sessions-16-multilevel-model-specification-lme4/

############################################################
# SETTING ENVIRONMENT
############################################################
library(lme4)
library(mlmRev)

############################################################
# MULTILEVEL MODELING
############################################################

# model with 
lmer(normexam ~ standLRT + (1 | school), data=Exam)

lmer(normexam ~ standLRT + (standLRT | school), 
	data=Exam, method=”ML”)

lmer(normexam ~ standLRT * schavg + (1 + standLRT | school), 
	data=Exam)

#intervals(m1.lme4)
summary(m1.lme4)
anova(m1.lme4)
#generating coefficients of whatever
coefs <- data.frame(coef(summary(m1.lme4)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
# get Satterthwaite-approximated degrees of freedom
coefs$df.Satt <- coef(summary(m1.lme4))[, 3]
# get approximate p-values
coefs$p.Satt <- coef(summary(m1.lme4))[, 5]
coefs
difflsmeans(m1.lme4)
plot(difflsmeans(m1.lme4, test.effs="groups:moments"))
lsmeans(m1.lme4)
confint(m1.lme4,level=0.95)
lmmpower(m1.lme4, pct.change = 0.30, t = seq(0,5,1), power = 0.80)

m1.lme4_2 = lmer(outcome ~ groups + moments + (1|subject),data = nlmedata,REML=FALSE)

m1.lme4_3 = lmer(outcome ~ groups + (1|subject),data = nlmedata,REML=FALSE)

anova(m1.lme4,m1.lme4_2,m1.lme4_3)

lmmpower(m1.lme4, pct.change = 0.40, t = seq(0,3,1), power = 0.80)

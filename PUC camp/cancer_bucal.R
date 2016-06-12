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
lapply(c("epicalc", "sem","Hmisc","ggplot2", "psych", "irr", 
	"nortest", "moments","GPArotation","nFactors","repmis",
	"gdata","qgraph","igraph","meta","metafor"), 
library, character.only=T)
#################################################################
#IMPORTING DATA
#################################################################
#LOADING DATA FROM A .CSV FILE
data<-read.csv("/Users/jnv4/Desktop/data_cancerbucal_UF.csv",sep=",")
#information between " " are the path to the directory in your computer 
#where the data is stored

#################################################################
#MANAGING DATA
#################################################################

#extracting the data involved with the analysis
data_cancerbucal<-with(data,data.frame(idh,
	gini,
	renda_domic_pc,
	razao_sexo,
	X_pop60mais,
	tabagismo_ps,
	adeq_equipam,
	adeq_exame_c,
	adeq_epi,
	adeq_rh_sb1,
	adeq_rh_sb2,
	vinculo_1,
	financ_ab,
	esf_cobertura,
	sb_cobertura,
	X_1_consulta_piab,
	acoes_cancer,
	med_od_bas_ind_piab,
	med_escov_d_super_coap,
	tx_mort_cb_100mil,
	tx_incid_cb_100mil,
	ano2,
	regiao))

#################################################################
#GRAPH ANALYSIS
#################################################################

cor<-cor_auto(data_cancerbucal)


graph_glasso<-qgraph(cor,layout="spring",vsize=6,
	esize=20,graph="glasso",sampleSize=nrow(data_cancerbucal),
	legend.cex = 0.5,GLratio=1.5)
graph_pcor<-qgraph(cor,layout="spring",vsize=6,
	esize=20,graph="pcor",threshold="holm",
	sampleSize=nrow(data_cancerbucal),legend.cex = 0.5,
	GLratio=1.5)
graph_cor<-qgraph(cor,layout="spring",vsize=6,
	esize=20,legend.cex = 0.5,GLratio=1.5)
Lqsg<-averageLayout(graph_glasso,graph_pcor,graph_cor)


qsgG3<-qgraph(cor,layout=Lqsg,nodeNames=colnames(data_cancerbucal),
	vsize=6,esize=20,
	graph="glasso",sampleSize=nrow(data_cancerbucal),
	legend.cex = 0.3,cut = 0.1, maximum = 1, minimum = 0, 
	repulsion = 0.8)#,gray=T,color=c("gray80","gray50"))
	#,groups=qsggr

#################################################################
#PRINCIPAL COMPONENTS
#################################################################

# # Define the amout of factor to retain
#Group of functinos to determine the number os items to be extracted
#par(mfrow=c(2,2)) #Command to configure the plot area for the scree plot graph
#ev <- eigen(cor_data) # get eigenvalues - insert the data you want to calculate the scree plot for
#ev # Show eigend values
#ap <- parallel(subject=nrow(cor_data),var=ncol(cor_data),rep=100,cent=.05) #Calculate the acceleration factor
#summary(ap)
#nS <- nScree(ev$values) #Set up the Scree Plot 
#plotnScree(nS) # Plot the ScreePlot Graph
#my.vss <- VSS(cor_data,title="VSS of BEA data")
#print(my.vss[,1:12],digits =2)
#VSS.plot(my.vss, title="VSS of 24 mental tests")
#scree(cor_data)
#VSS.scree(cor_data)
#fa.parallel(cor_data,n.obs=36)

# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 
fit <- principal(cor_data,4,rotate="varimax",scores=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
fit$scores
predict(fit,cor_data)
scores<-scoreItems(fit$weights,pca_data)
describe(scores$scores)
by(scores$scores,data_bea$risk_classification,summary)
wilcox.test(scores$scores[,1]~data_bea$risk_classification)
wilcox.test(scores$scores[,2]~data_bea$risk_classification)
wilcox.test(scores$scores[,3]~data_bea$risk_classification)
#wilcox.test(scores$scores[,4]~data_bea$risk_classification)

#################################################################
#MIXED MODELS
#################################################################
#aov.out = aov(outcome ~ groups + Error(subject/moments), data=nlmedata)
#summary(aov.out)
#nlmedata<-na.omit(nlmedata)
#with(nlmedata, t.test(outcome, groups,p.adjust.method="holm", paired=F))

nlmedata<-na.omit(nlmedata)
m1.lme4 = lmer(tx_mort_cb_100mil ~ ano2 + (1|regiao),
	data = data_cancerbucal,REML=FALSE)
#m1.nlme = lme(outcome ~ moments*groups,
#              random = ~ 1|subject/moments/groups,
#              data = nlmedata)
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



#applying weights using the "survey" package
#see:http://faculty.washington.edu/tlumley/old-survey/index.html
#see:https://rpubs.com/corey_sparks/53683
#see:https://www.r-bloggers.com/social-science-goes-r-weighted-survey-data/
#see:http://docs.zeligproject.org/articles/zelig_logitsurvey.html
data_who_weighted <- svydesign(ids = ~1, 
                     data = data_who,
                     weights = data_who$weights)






install.packages("ICC.Sample.Size")

library(ICC.Sample.Size)


## Calculate Sample Size for p=0.80, p0=0.60, two ratings, alpha=0.05 with two tails and power=0.80.
calculateIccSampleSize(p=0.90,p0=0.40,k=3,alpha=0.05,tails=2,power=0.80)
## Calculate Sample Size as above, but test varying p from 0 to 1 by steps of 0.05
calculateIccSampleSize(p=0.80,p0=0.60,k=2,alpha=0.05,tails=2,power=0.80,by="p",step=0.05)
## Calculate Sample Size as above, but test varying p0 from 0 to 1 by steps of 0.05
calculateIccSampleSize(p=0.80,p0=0.60,k=2,alpha=0.05,tails=2,power=0.80,by="p0",step=0.05)
## Calculate Sample Size as above, but test varying both p and p0 from 0 to 1 by steps of 0.05
calculateIccSampleSize(p=0.80,p0=0.60,k=2,alpha=0.05,tails=2,power=0.80,by="both",step=0.05)
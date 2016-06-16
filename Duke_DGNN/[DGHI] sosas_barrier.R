# install.packages("readstata13")
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



# To specify a latent class model, poLCA uses the standard, symbolic R model formula expres- sion. The response variables are the manifest variables of the model. Because latent class models have multiple manifest variables, these variables must be “bound” as cbind(Y1, Y2, Y3, ...) in the model formula. For the basic latent class model with no covariates, the formula definition takes the form
f <- cbind(PURPOSE, ACCURACY, UNDERSTA, COOPERAT) ~ 1

# The ~ 1 instructs poLCA to estimate the basic latent class model. For the latent class regres- sion model, replace the ~ 1 with the desired function of covariates, as, for example:
# f <- cbind(Y1, Y2, Y3) ~ X1 + X2 * X3

# To estimate the specified latent class model, the default poLCA command is:
# poLCA(formula, data, nclass = 2, maxiter = 1000, graphs = FALSE,
#     tol = 1e-10, na.rm = TRUE, probs.start = NULL, nrep = 1,
#     verbose = TRUE, calc.se = TRUE)

gss.lc2 <- poLCA(f, gss82, nclass = 2)

#nclass = number of latent classes to assume
#maxiter = maximum of iterations
#graph = logical weather poLCA should graphcially display the parameters
#tol=tolerance for judging convergence
#na.rm=exclude missing data
#probs.start=starter conditions for estimation
#verbose = display the results
#calc.se=calculate standard errors

gss.lc2$predcell

poLCA.table(formula = COOPERAT ~ 1,
+    condition = list(PURPOSE = 3, ACCURACY = 1, UNDERSTA = 2),
+    lc = gss.lc2)


# Entropy








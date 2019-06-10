##########################
# TEMPLATE CODE
###########################
# where df means data frame, and var1 / var2 are variables 

## IMPORT DATA
df <- read.csv("/Users/file_path/file_name.csv")  # for csv files
load("/Users/file_path/file_name.RData") # for R data

## SETTING THE ENVIRONMENT
install.packages("new_package") #only have to install once
library(new_package) #have to run every time

### DATA MANAGEMENT  

# obtain a subset of your sample (only pediatric patients for example)
df<-subset(df,age<19)

# recoding a variable's values from "1, 2, 3" to "A, B, C"
df$var1 <-car::recode(df$var1,"1='A'; 2='B';3='C'")

#recoding a variable to a factor
df$var <- as.factor(df$var)
#recoding a variable to an integer
df$var <- as.numeric(df$var)

### DESCRIPTIVE STATISTICS
#gives mean, median, min, max, and IQR
summary(df$var1)
mean(df$var1) #gives just the mean
sd(dfvar1)    #gives just standard deviation

#allows you to see how many observations are in a given category for a variable
table(df$var1)

### DATA ANALYSIS

# t test for continusous variables
t.test(df$var1~df$var2)
# chi sqaured test for categorical variables (input to chi square must be a table)
vars=table(df$var1, df$var2)
chisq.test(vars)

#linear regression (for continuous variables)
model <- glm(outcome ~ 
                    var1 +
                    var2 +
                    var3,
                  data = df,family = gaussian(link="identity"))

# logistic regression (for categorical variables)
model <- glm(outcome ~ var1 +
                       var2 +
                       var3,
             data = df, family = binomial(link = logit))
# prints logistic regression results and gives coefficients of model
summary(model1)
# transforms coefficients into odds ratios and rounds to three decimal places
round(exp(cbind(Odds=coef(model),confint(model,level=0.95))), 3)

### DATA VISUALIZATION 

#boxplot (don't forget to load the ggplot library)
ggplot(df, aes(x=var1, y=var2)) +
  geom_boxplot() +
  labs(title = "Boxplot")

ggplot(df, aes(x=var1, fill = var2))+
  geom_histogram(binwidth = 0.5) +
  labs(title = "Histogram", x = "Variable 1", y = "Variable 2")






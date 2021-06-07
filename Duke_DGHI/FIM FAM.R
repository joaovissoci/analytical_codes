## Ft João ##

setwd("C:/Users/Marcela Sanseverino/Desktop/FT João")

library(mirt)
library(readxl)
library(psych)
library(eRm)
library(dplyr)
library(tidyr)
library(nFactors)
library(qgraph)

# Read data
data <- read_xlsx(file.choose())

# Filter for the first evaluation and select items scores
data_validation <- data %>% 
  filter(redcap_event_name=="enrollment_arm_1") %>% 
  select(g1:g30)

# exlude all missing data
data_validation <- drop_na(data_validation)

# Check items distribution and correlation among them
describe(data_validation) # psych package 
cor_auto(data_validation) # qgraph package

# Distribuition visualization
bxFIM <- boxplot(data_validation$g1,
                 data_validation$g2,
                 data_validation$g3,
                 data_validation$g4,
                 data_validation$g5,
                 data_validation$g6,
                 data_validation$g7,
                 data_validation$g8,
                 data_validation$g9,
                 data_validation$g10,
                 data_validation$g11,
                 data_validation$g12,
                 data_validation$g13,
                 data_validation$g14,
                 data_validation$g15,
                 data_validation$g16,
                 data_validation$g17,
                 data_validation$g18,
                 data_validation$g19,
                 data_validation$g20,
                 data_validation$g21,
                 data_validation$g22,
                 data_validation$g23,
                 data_validation$g24,
                 data_validation$g25,
                 data_validation$g26,
                 data_validation$g27,
                 data_validation$g28,
                 data_validation$g29,
                 data_validation$g30,
                 main="Items distribuition",
                 boxfill = "darkgreen",
                 xlab = "Liker Scale",
                 ylab = "Items",
                 lex.order = TRUE
)

# One Factor Model
mod1 <- mirt(data_validation,1,itemtype = "graded")
summary(mod1)
coef(mod1) # items' discrimination parameter and easiness
M2(mod1) # model fit indexes 

# Two Factor Model
mod2 <- mirt(data_validation,2,itemtype = "graded")
summary(mod2)
coef(mod2) # items' discrimination parameter and easiness
M2(mod2) # model fit indexes 

# Comparing two models
anova(mod1, mod2)

# Empirical reliability of One Factor Model
Mod1.REL <- fscores(mod1, method= "EAP", returnER = T)
Mod1.REL
install_github("paul-buerkner/brms")

library(devtools)
library("brms")
data("kidney")
head(kidney, n = 3)

fit1 <- brm(formula = time | cens(censored) ~ age + sex + disease 
            + (1 + age|patient), 
            data = kidney, family = gaussian("log"),
            prior = c(set_prior("normal(0,5)", class = "b"),
                      set_prior("cauchy(0,2)", class = "sd"),
                      set_prior("lkj(2)", class = "cor")),
            warmup = 1000, iter = 2000, chains = 4,
            control = list(adapt_delta = 0.95))


fit2 <- brm(formula = time | cens(censored) ~ age + sex + disease 
            + (1|patient), 
            data = kidney, family = gaussian("log"),
            prior = c(set_prior("normal(0,5)", class = "b"),
                      set_prior("cauchy(0,2)", class = "sd")),
            warmup = 1000, iter = 2000, chains = 4,
            control = list(adapt_delta = 0.95))

prior <- c(set_prior("normal(0,10)", class = "b", coef = "age"),
           set_prior("cauchy(1,2)", class = "b", coef = "sexfemale"))
summary(fit1, waic = TRUE)
hypothesis(fit1, "Intercept - age > 0", class = "sd", group = "patient")
LOO(fit1, fit2)
data("inhaler")
head(inhaler, n = 1)
fit3 <- brm(formula = rating ~ treat + period + carry + (1|subject), 
            data = inhaler, family = cumulative)
fit4 <- brm(formula = rating ~ period + carry + (1|subject),
            data = inhaler, family = sratio,
            partial = ~ treat, threshold = "equidistant",
            prior = set_prior("normal(1,2)", coef = "treat"))
summary(fit4, waic = TRUE)
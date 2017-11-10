install.packages("MKmisc")
library("MKmisc")


## see n2 on page 1202 of Chu and Cole (2007)
power.diagnostic.test(spec = 0.95,
					  delta = 0.1,
					  power = 0.95, 
					  sig.level=0.01,
					  prev=0.07) # 40

## see n2 on page 1202 of Chu and Cole (2007)
power.diagnostic.test(spec = 0.95,
					  delta = 0.1,
					  power = 0.95, 
					  sig.level=0.01,
					  prev=0.03) # 40
# power.diagnostic.test(sens = 0.99, delta = 0.13, power = 0.95) # 43
# power.diagnostic.test(sens = 0.99, delta = 0.12, power = 0.95) # 47

## see n2 on page 1202 of Chu and Cole (2007)
power.diagnostic.test(sens = 0.99,
					  delta = 0.1,
					  power = 0.95, 
					  sig.level=0.05,
					  prev=0.03) # 40


For sample size, in the first study (adult rule), out of 3435 adults, 311 (9.1%) had abdominal injuries, and 109 (3%) required acute intervention (surgery or angiographic embolization).

a.      For free fluid, LR+ was 36, LR- 0.24
b.      In hypotensive patients (the sickest part of the shock population), the sensitivity was >90% (fig 4 b).
c.      LR negative for abdominal organ injuries was 0.21.
d.      Sensitivity/specificity for organ injuries (tables 3, 5).  In general, high specificity (>95%), low sensitivity (15-38%).
e.      As an estimate of the prevalence of injury, table 5 lists spleen as 7%, liver 3%, kidney 3%, small bowel 1%, free fluid 16-19%.  Our numbers might be higher if our inclusion criteria focus on shock.
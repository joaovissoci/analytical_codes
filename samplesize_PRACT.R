
install.packages("ldbounds")

library(ldbounds)


t <- seq(0.2,1,length=6)
obf.bd <- bounds(t,iuse=c(1,1),alpha=c(0.025,0.025))
summary(obf.bd)
plot(obf.bd)

library(pwr)
pwr.t.test(n=21,d=NULL,power=0.80,sig.level=1.0777e-06)
pwr.t.test(n=43,d=NULL,power=0.80,sig.level=3.7335e-04)
pwr.t.test(n=64,d=NULL,power=0.80,sig.level=3.3893e-03)
pwr.t.test(n=85,d=NULL,power=0.80,sig.level=9.3678e-03)
pwr.t.test(n=128,d=NULL,power=0.80,sig.level=1.5793e-02)
pwr.t.test(n=140,d=NULL,power=0.80,sig.level=2.1075e-02)


pwr.t.test(n=140,d=NULL,power=0.80,sig.level=0.05)


pwr.t.test(n=21,d=NULL,power=0.80,sig.level=1.0777e-06)
pwr.t.test(n=63,d=NULL,power=0.80,sig.level=3.7335e-04)
pwr.t.test(n=104,d=NULL,power=0.80,sig.level=3.3893e-03)
pwr.t.test(n=146,d=NULL,power=0.80,sig.level=9.3678e-03)
pwr.t.test(n=182,d=NULL,power=0.80,sig.level=1.5793e-02)
pwr.t.test(n=260,d=NULL,power=0.80,sig.level=2.1075e-02)


pwr.t.test(n=260,d=NULL,power=0.80,sig.level=0.05)








pwr.t.test(n=21,d=NULL,power=0.80,sig.level=.05,alternative="greater")
pwr.t.test(n=140,d=NULL,power=0.80,sig.level=.025,alternative="greater")
pwr.t.test(n=NULL,d=0.30,power=0.80,sig.level=.025,alternative="greater")
pwr.t.test(n=NULL,d=0.30,power=0.80,sig.level=.05,alternative="greater")


t <- c(0.2292,0.3333,0.4375,0.5833,0.7083,0.8333)
t2 <- c(56,77,126,177,247,318)
power.fam <- bounds(t,t2,iuse=c(1,1),alpha=c(0.025,0.025))
summary(power.fam)


## Using output from 'bounds'
t <- seq(0.2,1,length=5)
obf.bd <- bounds(t,iuse=c(1,1),alpha=c(0.025,0.025))
drift.dr <- drift(obf.bd$lower.bounds,obf.bd$upper.bounds,t,pow=0.8)
summary(drift.dr)
plot(drift.dr)



install.packages("gsDesign")
library(gsDesign)

x <- gsDesign(k=5, test.type=2, beta=0.20,delta=0.3,sfu="OF")
summary(x)
print(x)

gsBoundCP(x)
gsBoundSummary(x)
gsProbability(x)

plot(x,plottype=1)
plot(x,plottype=2)
plot(x,plottype=3)
plot(x,plottype=4)
plot(x,plottype=5)
plot(x,plottype=6)
plot(x,plottype=7)
plot(x,plottype=8)
plot(x,plottype=9)

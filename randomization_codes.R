library("randomizeR")

params<-rpbrPar(20, 10)

R <- genSeq(params)

getRandList(R)

plotSeq(R)


N <- 10
params <- crPar(N)
# params <- crPar(N)
R <- genSeq(params)
R


seed=42
blocksize = 5
N = 30
set.seed(seed)
block = rep(1:ceiling(N/blocksize), each = blocksize)
a1 = data.frame(block, rand=runif(length(block)), envelope= 1: length(block))
a2 = a1[order(a1$block,a1$rand),]
a2$arm = rep(c("Arm 1", "Arm 2", "Arm 3"),times = length(block)/3)
assign_phase1 = a2[order(a2$envelope),]


assign_phase1

write.csv(assign_phase1,"/Users/Joao/Desktop/phase1_randomization.csv")


seed=42
blocksize = 5
N = 25
set.seed(seed)
block = rep(1:ceiling(N/blocksize), each = blocksize)
a1 = data.frame(block, rand=runif(length(block)), envelope= 1: length(block))
a2 = a1[order(a1$block,a1$rand),]
a2$arm = rep(c("Arm 1", "Arm 2", "Arm 2", "Arm 2", "Arm 2"),times = length(block)/5)
assign_phase3 = a2[order(a2$envelope),]


assign_phase3

write.csv(assign_phase3,"/Users/Joao/Desktop/phase3_randomization.csv")


seed=42
blocksize = 5
N = 20
set.seed(seed)
block = rep(1:ceiling(N/blocksize), each = blocksize)
a1 = data.frame(block, rand=runif(length(block)), envelope= 1: length(block))
a2 = a1[order(a1$block,a1$rand),]
a2$arm = rep(c("Arm 1", "Arm 2"),times = length(block)/2)
assign_phase2 = a2[order(a2$envelope),]


assign_phase2

write.csv(assign_phase2,"/Users/Joao/Desktop/phase2_randomization.csv")


require(devtools)
install_github('likert', 'jbryer')
library("likert")
data(pisaitems)

items28 <- pisaitems[, substr(names(pisaitems), 1, 5) == "ST24Q"]
head(items28)
colnames(items28)<- c(
	ST24Q01 = "I read only if I have to.", 
	ST24Q02 = "Reading is one of my favorite hobbies.", 
    ST24Q03 = "I like talking about books with other people.", 
    ST24Q04 = "I find it hard to finish books.", 
    ST24Q05 = "I feel happy if I receive a book as a present.", 
    ST24Q06 = "For me, reading is a waste of time.", 
    ST24Q07 = "I enjoy going to a bookstore or a library.", 
    ST24Q08 = "I read only to get information that I need.", 
    ST24Q09 = "I cannot sit still and read for more than a few minutes.", 
    ST24Q10 = "I like to express my opinions about books I have read.", 
    ST24Q11 = "I like to exchange books with my friends")

l28 <- likert(items28)
summary(l28)

plot(l28, include.histogram = TRUE)

plot(l28, centered = FALSE, wrap = 30)

plot(l28, type = "density")

plot(l28, type = "heat")

l28g <- likert(items28, grouping = pisaitems$CNT)

plot(l28g, include.histogram = TRUE)

plot(l28g, centered = FALSE)

plot(l28g, type = "density")

items29 <- pisaitems[, substr(names(pisaitems), 1, 5) == "ST25Q"]

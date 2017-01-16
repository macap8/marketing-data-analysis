rm(list=ls())
setwd("~/Disk Google/Repositories/marketing-data-analysis/Yoghurt")
getwd()

#Installed packages
install.packages("binom")

#Libraries
library(binom)

load("Yoghurt.RData")
str(yoghurt.df)

round(prop.table(table(yoghurt.df$gender, yoghurt.df$ben_live)), 2)
round(prop.table(table(yoghurt.df$gender)), 2)
round(prop.table(table(yoghurt.df$ben_live)), 2)


#testing the distribution 
tmp.tab <- table(rep(c(1:6), times=c(50, 50, 50, 50, 50, 45)))
chisq.test(tmp.tab)
qchisq(.95, df=5) 
Chi2Distr <- rchisq(295, 5)
hist(Chi2Distr, prob=TRUE)
curve( dchisq(x, df=5), col='green', add=TRUE)
# abline(h=0.95, lty=3)
abline(v=quantile(Chi2Distr, pr=0.95), lty=3)


plot.new()
chisq.test(table(yoghurt.df$gender, yoghurt.df$income))
qchisq(.95, df=9)
Chi2Distr <- rchisq(2123, 9)
hist(Chi2Distr, prob=TRUE)
curve(dchisq(x, df=9), col='green', add=TRUE)
abline(v=quantile(Chi2Distr, pr=0.95), lty=3)

gender.df <- as.data.frame(table(yoghurt.df$gender))
Male <- gender.df[1,2]
Female <- gender.df[2, 2]
binom.test(Male, (Male+Female), p=0.5)

binom.confint(Male, (Male+Female), method="ac")

set.seed(1601)
nresp <- 2123
yoghurt.df$income.av <- rnorm(nresp, mean=62500, sd=0.4)

require(lattice)
histogram(~income.av|gender, data=yoghurt.df)

yoghurt.df$income.av[yoghurt.df$income=="15K-25K"] <- 20000
yoghurt.df$income.av[yoghurt.df$income=="25K-35K"] <- 30000
yoghurt.df$income.av[yoghurt.df$income=="35K-50K"] <- 40000
yoghurt.df$income.av[yoghurt.df$income=="50K-75K"] <- 60000
yoghurt.df$income.av[yoghurt.df$income=="75K-100K"] <- 85000
yoghurt.df$income.av[yoghurt.df$income=="100K-150K"] <- 125000
yoghurt.df$income.av[yoghurt.df$income=="150K-200K"] <- 175000
yoghurt.df$income.av[yoghurt.df$income==">200K"] <- 200000
yoghurt.df$income.av[yoghurt.df$income=="No answer"] <- NA

# yoghurt.df <- read.csv("Yoghurt_Data.csv")
# str(yoghurt.df)


qqnorm(yoghurt.df$income.av)
qqline(yoghurt.df$income.av)

t.test(income.av ~ gender, data=yoghurt.df)

#Formalize the result and Data Visualization

library(Rmisc)
library(ggplot2)
library(plotly)
library(reshape2)
library(dplyr)
library(gmodels)
library(ggplot2)
library(readr)



#baseline Penalized Cox

load("~/Dropbox/2020 Fall/deepsurvresults/11.18.m1.noscale.adjust.ridge.Rda")

base = df0

citrain_base = group.CI(train ~ type , data = base, ci = 0.95)
citest_base  = group.CI(test ~ type , data = base, ci = 0.95)
cinumcov_base = group.CI(num_cov ~ type , data = base, ci = 0.95)
citrain_base 
citest_base 
cinumcov_base

basege  = base[base$type == "geOS",]$test

#tf-Penalized Cox
load("~/Dropbox/2020 Fall/deepsurvresults/11.17.m2.tensor.ridge.adjust.noscale-200.10y-run2.Rda")
tensorpc = df1

os_tensorpc = tensorpc[tensorpc$type == "OS",]
dss_tensorpc = tensorpc[tensorpc$type == "DSS",]

citrain_tensorpc = group.CI(train ~ rank , data = os_tensorpc, ci = 0.95)

citest_tensorpc = group.CI(test ~ rank, data = os_tensorpc, ci = 0.95)

cinumcov_tensorpc = group.CI(num_cov ~ rank , data = os_tensorpc, ci = 0.95)

maxid_tensorpc = which.max(citest_tensorpc$test.mean)


citrain_tensorpc[maxid_tensorpc,]
citest_tensorpc[maxid_tensorpc,]
cinumcov_tensorpc[maxid_tensorpc,]

tpc17 = os_tensorpc[os_tensorpc$rank == "17",]$test



#plot Cindex VS ranks
ranks = seq(2,200)


plot1 <- data.frame(Types=rep(c("Train", "Test"), each=199),
                    ranks=rep(seq(2,200),2),
                    cindex=c(citrain_tensorpc$train.mean,citest_tensorpc$test.mean),
                    ciupper= c(citrain_tensorpc$train.upper,citest_tensorpc$test.upper),
                    cilower= c(citrain_tensorpc$train.lower,citest_tensorpc$test.lower)
)


ggplot(data=plot1, aes(x=ranks, y=cindex, group=Types)) +
  geom_line(aes(color = Types))+
  geom_point() +
  theme_grey(base_size = 15) +
  geom_ribbon(data=plot1,aes(ymin=cilower,ymax=ciupper),alpha=0.2) +
  labs(title="TF-Penalized Cox : Survival Results of Rank 2 to 200 factorization",x="Rank", y = "C-index")




#deepsurvs

geds = read_csv("~/Dropbox/2020 Fall/deepsurvresults/12.20deepsurvGE.csv")
citrain_geds = group.CI(trainCI ~ rank  , data = geds, ci = 0.95)
citest_geds = group.CI(testCI ~ rank  , data = geds, ci = 0.95)
citrain_geds 
citest_geds

ds = geds$testCI

#tfdeepsurv

tfdeepsurv = read_csv("~/Dropbox/2020 Fall/deepsurvresults/12.20deepsurvMLPranklist17.csv")
citrain_tfds = group.CI(trainCI ~ rank  , data = tfdeepsurv, ci = 0.95)
citest_tfds = group.CI(testCI ~ rank  , data = tfdeepsurv, ci = 0.95)
citrain_tfds 
citest_tfds

tfds17 = tfdeepsurv$testCI


#DeepTensorSurvival(DTS)
logh17 = read_csv("~/Dropbox/2020 Fall/deepsurvresults/11.21.auto_loghazardrank17.csv")
citrain_dts = group.CI(trainCI ~ rank  , data = logh17, ci = 0.95)
citest_dts = group.CI(testCI ~ rank  , data = logh17, ci = 0.95)
citrain_dts 
citest_dts
dts = logh17$testCI



#wilcoxon

library(ggpubr)


facbase = tensorfac17-basege

#
wilcox.test(tpc17-basege, mu = 0, alternative = "greater")

wilcox.test(ds-basege, mu = 0, alternative = "greater")

wilcox.test(tfds17-basege, mu = 0, alternative = "greater")

wilcox.test(dts-basege, mu = 0, alternative = "greater")




















#wilcoxon

library(ggpubr)


facbase = tensorfac17-basege

#
wilcox.test(tensorfac17-basege, mu = 0, alternative = "greater")

wilcox.test(ib015-basege, mu = 0, alternative = "greater")

wilcox.test(deepsurv-basege, mu = 0, alternative = "greater")

wilcox.test(logh-basege, mu = 0, alternative = "greater")




interval<- read_csv("Dropbox/2020 Fall/deepsurvresults/1.17.auto_loghazardrank17intervals5-101.csv")

xlist = seq(5,101,5)

Mean_TestCI = citestcoxsimp$testCI.mean

plot(xlist,Mean_TestCI,xlab = "number of Time Intervals", ylab = "Mean Test C-indexes")




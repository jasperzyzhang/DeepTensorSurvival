#import libraries needed
library(tidyverse)
library(survival)
library(ggplot2)
library(ggfortify)
library(dplyr)
library("glmnet")
library(coxphf)
library(caret)
library(boot)
library(rlist)


#separate data into train/test set by predefined index
sep_data <- function(alldata, ind) {
  
  sets = list()
  
  #ind
  sets[[1]] = alldata[ind,]
  #data rest
  sets[[2]] = alldata[-ind,]
  
  return(sets)
}


#set penalty factor, Then Lasso/Ridge will not penalize last two columns(Age and Stage)
pf <- function(data) {
  
  
  return(c(rep(1,ncol(data)-2),0,0))
}



#path to load datasets
dirpath <- "D:\\practicum\\ndtf\\"
datapath <- paste(dirpath,"new_datasets\\",sep = "")
facpath <- paste(dirpath,"factorized_data_\\",sep = "")
indpath <- paste(dirpath,"ind100\\",sep = "")
resultpath <- paste(dirpath,"results\\",sep = "")


filename = "ridge_GE.Rda"


#load datasets
clinical_data <- read.csv(paste(datapath,"new_clinical_10y.csv",sep="") ,header = TRUE)
ge <- read.csv(paste(datapath,"new_ge.csv",sep="") ,header = TRUE)
cn <- read.csv(paste(datapath,"new_cn.csv",sep="") ,header = TRUE)
me <- read.csv(paste(datapath,"new_me.csv",sep="") ,header = TRUE)
con <- cbind(ge,cn,me) #dataconcatenation
time = clinical_data$OS.time
status = clinical_data$OS

#add two cols(Age and Stage to datasets)
ge$agegroup = clinical_data$agegroup
cn$agegroup = clinical_data$agegroup
me$agegroup = clinical_data$agegroup

ge$stagegroup = clinical_data$stagegroup
cn$stagegroup = clinical_data$stagegroup
me$stagegroup = clinical_data$stagegroup

con$agegroup = clinical_data$agegroup
con$stagegroup = clinical_data$stagegroup








PenalizedCox <- function(data, time, status, trainind,alpha = 1,df){
  
  train_time = time[trainind]
  test_time = time[-trainind]
  train_status = status[trainind]
  test_status = status[-trainind]
  
  split_data = sep_data(data,trainind)
  train <- split_data[[1]]
  test<- split_data[[2]]
  traindata <- as.matrix(train)
  trainsurv <- Surv(train_time,train_status)
  testdata <- as.matrix(test)
  testsurv <- Surv(test_time,test_status)
  set.seed(2020)
  try({ fit.lasso <- cv.glmnet(traindata, trainsurv, alpha = alpha, family = "cox",type.measure = "C",maxit = 1000,penalty.factor = pf(traindata))
  
  Coefficients = coef(fit.lasso, s = "lambda.min")
  Active.Index <- which(Coefficients != 0)
  
  pred1 = predict(fit.lasso, newx = traindata)
  pred2 = predict(fit.lasso, newx = testdata)
  
  cindex1 = Cindex(pred1, trainsurv)
  cindex2 = Cindex(pred2, testsurv)
  })
  
  df = rbind(df, data.frame(seed = s, type = paste(dt,st,sep = ""), train = cindex1, test = cindex2,num_cov = length(Active.Index)))
  
  return(df0)
  }

#Main procedure:

for (s in c(1:20)) {
  
  print(s)
  
  trainind <- read.csv(paste(indpath,"trainind", s, ".csv",sep="") ,header = TRUE)
  trainind <- trainind$x
  
  df0 <- data.frame(seed = numeric(), type = character(), train = numeric(), test = numeric(),num_cov = numeric())
  
  df0 = PenalizedCox(ge, time,status, trainind,1,df0)
  
}



save(df0,file = paste(resultpath,filename,sep = ""))


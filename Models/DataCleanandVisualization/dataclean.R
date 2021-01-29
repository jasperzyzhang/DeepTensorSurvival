#Data Clean
library(dplyr)
library(car)


nih <- read.csv("~/Dropbox/2020 Spring/ndtf/old_datasets/NIH.csv",header = TRUE,stringsAsFactors = F)
clinical_data <- read.csv("~/Dropbox/2020 Spring/ndtf/old_datasets/FinalClinicalData.csv",header = TRUE,stringsAsFactors = F)
ge <- read.csv("~/Dropbox/2020 Spring/ndtf/old_datasets/FinalGeneExpressionData.csv",header = TRUE,row.names = 1)
cn <- read.csv("~/Dropbox/2020 Spring/ndtf/old_datasets/FinalCopyNumberVariationData.csv",header = TRUE,row.names = 1)
me <- read.csv("~/Dropbox/2020 Spring/ndtf/old_datasets/FinalMethylationData.csv",header = TRUE,row.names = 1)

clinical_data$bcr_patient_barcode = as.character(clinical_data$bcr_patient_barcode)
nih$bcr_patient_barcode = as.character(nih$bcr_patient_barcode)

clinical_merge = merge(clinical_data,nih,by = "bcr_patient_barcode",sort = FALSE)

nih_select = clinical_merge %>% select(bcr_patient_barcode,age_at_initial_pathologic_diagnosis,ajcc_pathologic_tumor_stage.y,OS,OS.time,DSS,DSS.time)

nih_select[nih_select == "#N/A"] = NA

nih_select[nih_select == "[Not Available]"] = NA
nih_select[nih_select == "[Discrepancy]"] = NA

nih_select$DSS[is.na(nih_select$DSS)] = 0
nih_select$OS[is.na(nih_select$OS)] = 0

clean_nih = na.omit(nih_select)

names(clean_nih)[1] <- "Barcode"
names(clean_nih)[2] <- "age"
names(clean_nih)[3] <- "stage"


factor_to_num <- function(data) {
  return(as.numeric(as.character(data)))
}

clean_nih$age <- factor_to_num(clean_nih$age)
clean_nih$OS <- factor_to_num(clean_nih$OS)
clean_nih$OS.time <- factor_to_num(clean_nih$OS.time)
clean_nih$DSS <- factor_to_num(clean_nih$DSS)
clean_nih$DSS.time <- factor_to_num(clean_nih$DSS.time)
clean_nih$agegroup = ifelse(clean_nih$age >40,1,0)

clean_nih$stagegroup = recode(clean_nih$stage,"c('Stage I','Stage IA', 'Stage IB','Stage II','Stage IIA', 'Stage IIB') = 'low' 
                              ; c('Stage III','Stage IIIA', 'Stage IIIB','Stage IIIC','Stage X', 'Stage IV') = 'high'")

clean_nih$stagegroup = ifelse(clean_nih$stagegroup == "low",0,1)


matchback = match(clean_nih$Barcode,clinical_data$bcr_patient_barcode)

new_ge = as.data.frame(t(ge[,matchback]))
new_me = as.data.frame(t(me[,matchback]))
new_cn = as.data.frame(t(cn[,matchback]))

nonzerocol <- function(data) {
  
  ind = seq(1,ncol(data))
  colsnum = ind[!apply(data == 0, 2, all)]
  
  return(colsnum)
}

gecol = nonzerocol(new_ge)

non0_ge = new_ge[,gecol]
non0_me = new_me[,gecol]
non0_cn = new_cn[,gecol]



write.csv(non0_ge,"new_ge.csv",row.names = F)
write.csv(non0_me,"new_me.csv",row.names = F)
write.csv(non0_cn,"new_cn.csv",row.names = F)



#change follow up time to 10-years(3650 days)

gt10y_ind = clean_nih$OS.time > 3650
gt10y = clean_nih[clean_nih$OS.time >= 3650,]

num10y = nrow(gt10y)
#25, 21 censoring, 4 events

clean_nih[gt10y_ind,]$OS.time <-  rep(3650,num10y)
clean_nih[gt10y_ind,]$OS <-  rep(0,num10y)
clean_nih[gt10y_ind,]$DSS.time <-  rep(3650,num10y)
clean_nih[gt10y_ind,]$DSS <-  rep(0,num10y)


write.csv(clean_nih,"new_clinical_10y.csv")



#randomly generate 100 trainig/testing pairs stratified

total_num = nrow(clean_nih)
allind = c(1:total_num)
event_ind = which(clean_nih$OS == 1)
censor_ind = which(clean_nih$OS == 0)

event_num = length(event_ind)
censor_num = length(censor_ind)

train_num = floor(total_num *2/3)
train_num_e = floor(event_num * (train_num / total_num))
train_num_c = train_num - train_num_e

for (s in seq(1,50)){
trainind <- read.csv(paste(indpath,"trainind", s, ".csv",sep="") ,header = TRUE)
trainind <- trainind$x
print(length(trainind))
tr = clean_nih[trainind,]
print(sum(tr$OS))

}


set.seed(100)
for(i in  c(1:100)){
  
  trainind_e = sample(event_ind, train_num_e)
  trainind_c = sample(censor_ind, train_num_c)
  trainind = c(trainind_e,trainind_c)
  
  testind = allind[-trainind]
  
  write.csv(trainind,paste("~/Dropbox/2020 Fall/ind100/", "trainind",i, ".csv",sep=""))
  write.csv(testind,paste("~/Dropbox/2020 Fall/ind100/", "testind",i, ".csv",sep=""))
}

for (ind in (c(1:100))){
for (rank in list){

  
  file.copy(paste("/Volumes/jasper/ndtf/factorized_data_noscale/", "test",ind,"rank",rank, ".csv",sep=""), paste("~/Dropbox/2020 Spring/deepsurv_DTF/facdata/"))
  file.copy(paste("/Volumes/jasper/ndtf/factorized_data_noscale/", "train",ind,"rank",rank, ".csv",sep=""),paste("~/Dropbox/2020 Spring/deepsurv_DTF/facdata/"))
  #test = read.csv(paste("/Volumes/jasper/ndtf/factorized_data_noscale/", "test",ind,"rank",rank, ".csv",sep=""),header = FALSE)
#train = read.csv(paste("/Volumes/jasper/ndtf/factorized_data_noscale/", "train",ind,"rank",rank, ".csv",sep=""),header = FALSE)

#write.csv(test,paste("~/Dropbox/2020 Spring/deepsurv_DTF/facdata_top10/", "test",ind,"rank",rank, ".csv",sep=""))
#write.csv(train,paste("~/Dropbox/2020 Spring/deepsurv_DTF/facdata_top10/", "train",ind,"rank",rank, ".csv",sep=""))
}}



for (ind in (c(1:20))){
  for (rank in seq(10,200,10)){
    
    
    file.copy(paste("/Volumes/jasper/ndtf/factorized_data_noscale/", "test",ind,"rank",rank, ".csv",sep=""), paste("~/Dropbox/2020 Spring/deepsurv_DTF/facdata/"))
    file.copy(paste("/Volumes/jasper/ndtf/factorized_data_noscale/", "train",ind,"rank",rank, ".csv",sep=""),paste("~/Dropbox/2020 Spring/deepsurv_DTF/facdata/"))
    #test = read.csv(paste("/Volumes/jasper/ndtf/factorized_data_noscale/", "test",ind,"rank",rank, ".csv",sep=""),header = FALSE)
    #train = read.csv(paste("/Volumes/jasper/ndtf/factorized_data_noscale/", "train",ind,"rank",rank, ".csv",sep=""),header = FALSE)
    
    #write.csv(test,paste("~/Dropbox/2020 Spring/deepsurv_DTF/facdata_top10/", "test",ind,"rank",rank, ".csv",sep=""))
    #write.csv(train,paste("~/Dropbox/2020 Spring/deepsurv_DTF/facdata_top10/", "train",ind,"rank",rank, ".csv",sep=""))
  }}



geee <- read.csv("new_ge.csv",header = TRUE)

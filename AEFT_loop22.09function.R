#working directory
setwd("~/files/AEFT")
#packages
library("dplyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
# first read in ALL .csv tables (local and global)!
# e.g. AR20RED171_AEFT <- read.csv("~/files/AGLT/AR20RED171_AEFT_2017_Jul_05_1408.csv")
##loop over all AGLT result files##



file_list_AEFT<-ls(pattern="[A-Z]{2}[0-9]{2}[A-Z]{3}[0-9]{3}_AEFT{1}")
AEFTloop<-function(filelist){
  AEFTresults<-numeric(16*length(filelist)) 
  for (i in 1:length(filelist)) {
    tableAEFT<-get(filelist[i])
    ins_0 <- AEFT_ins_0(tableAEFT)
    ins_6 <- AEFT_ins_6(tableAEFT)
    ins_12<-AEFT_ins_12(tableAEFT)
    ins_24<-AEFT_ins_24(tableAEFT)
    miss_0 <- AEFT_miss_0(tableAEFT)
    miss_6 <- AEFT_miss_6(tableAEFT)
    miss_12<-AEFT_miss_12(tableAEFT)
    miss_24<-AEFT_miss_24(tableAEFT)
    AEFTresults[((16*i)-15):(16*i)]<- c(ins_0,ins_6,ins_12,ins_24,miss_0,miss_6,miss_12,miss_24)
    i=i+1
  }
  dim(AEFTresults)<-c(16,length(filelist))
  
  colnames(AEFTresults)<-file_list_AEFT
  AEFTresults<-t(AEFTresults)
  colnames(AEFTresults)<-c("Ins_0_corr", "Ins_0_meanRT","Ins_6_corr", "Ins_6_meanRT","Ins_12_corr","Ins_12_meanRT","Ins_24_corr","Ins_24_meanRT", "miss_0_corr","miss_0_meanRT", "miss_6_corr", "miss_6_meanRT","miss_12_corr", "miss_12_meanRT","miss_24_corr", "miss_24_meanRT")
  assign("AEFTresults", AEFTresults, envir=globalenv())
  return (AEFTresults)
}
AEFTloop(file_list_AEFT)

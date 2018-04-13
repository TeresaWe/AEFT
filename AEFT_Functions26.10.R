
library("dplyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
###Functions###

##Inside
#Mean of Melodies, inside, 0ST separated

count=0
AEFT_ins_0<-function(AEFTtable){
  count=0
  trial=0
  Keyrt<-as.numeric(as.character(AEFTtable$time))
  Keycorr<-as.numeric(as.character(AEFTtable$correct.answer.))
  cond<-as.character(AEFTtable$cond)
  vec<-numeric()
  for (i in 17:144) {#skip practice trials
    if (Keycorr[i]==1 & cond[i]=="True_0"){ #only congruent and correct trials
      count=count+1
      trial=trial+1
      vec[i]<-Keyrt[i]
    }
    if (Keycorr[i]==0 & cond[i]=="True_0"){ #only congruent and correct trials
      count=count+0
      trial=trial+1
      vec<-vec
    }
    else{
      count=count+0
      vec<-vec
    }
    i=i+1
    Ins_0_meanRT<-mean(vec,na.rm=TRUE)
    assign("Ins_0_RT", z, envir=globalenv())
    assign("Ins_0_corr", count, envir=globalenv())
    assign("Ins_0_meanRT", Ins_0_meanRT, envir=globalenv())
    assign("N_Ins_0", trial,envir=globalenv())
  }
  return (c(Ins_0_corr,Ins_0_meanRT,N_Ins_0))
}


#Mean of Melodies, inside, 6ST separated

count=0
AEFT_ins_6<-function(AEFTtable){
  count=0
  trial=0
  Keyrt<-as.numeric(as.character(AEFTtable$time))
  Keycorr<-as.numeric(as.character(AEFTtable$correct.answer.))
  cond<-as.character(AEFTtable$cond)
  vec<-numeric()
  for (i in 17:144) {#skip practice trials
    if (Keycorr[i]==1 & cond[i]=="True_6"){ #only congruent and correct trials
      count=count+1
      trial=trial+1
      vec[i]<-Keyrt[i]
    }
    if (Keycorr[i]==0 & cond[i]=="True_6"){ #only congruent and correct trials
      count=count+0
      trial=trial+1
      vec<-vec
    }
    else{
      count=count+0
      vec<-vec
    }
    i=i+1
    Ins_6_meanRT<-mean(vec,na.rm=TRUE)
    assign("Ins_6_RT", z, envir=globalenv())
    assign("Ins_6_corr", count, envir=globalenv())
    assign("Ins_6_meanRT", Ins_6_meanRT, envir=globalenv())
    assign("N_Ins_6", trial,envir=globalenv())
  }
  return (c(Ins_6_corr,Ins_6_meanRT,N_Ins_6))
}


#Mean of Melodies, inside, 12ST separated
count=0
AEFT_ins_12<-function(AEFTtable){
  count=0
  trial=0
  Keyrt<-as.numeric(as.character(AEFTtable$time))
  Keycorr<-as.numeric(as.character(AEFTtable$correct.answer.))
  cond<-as.character(AEFTtable$cond)
  vec<-numeric()
  for (i in 17:144) {#skip practice trials
    if (Keycorr[i]==1 & cond[i]=="True_12"){ #only congruent and correct trials
      count=count+1
      trial=trial+1
      vec[i]<-Keyrt[i]
    }
    if (Keycorr[i]==0 & cond[i]=="True_12"){ #only congruent and correct trials
      count=count+0
      trial=trial+1
      vec<-vec
    }
    else{
      count=count+0
      vec<-vec
    }
    i=i+1
    Ins_12_meanRT<-mean(vec,na.rm=TRUE)
    assign("Ins_12_RT", z, envir=globalenv())
    assign("Ins_12_corr", count, envir=globalenv())
    assign("Ins_12_meanRT", Ins_12_meanRT, envir=globalenv())
    assign("N_Ins_12", trial,envir=globalenv())
  }
  return (c(Ins_12_corr,Ins_12_meanRT,N_Ins_12))
}


#Mean of Melodies, inside, 24ST separated

count=0
AEFT_ins_24<-function(AEFTtable){
  count=0
  trial=0
  Keyrt<-as.numeric(as.character(AEFTtable$time))
  Keycorr<-as.numeric(as.character(AEFTtable$correct.answer.))
  cond<-as.character(AEFTtable$cond)
  vec<-numeric()
  for (i in 17:144) {#skip practice trials
    if (Keycorr[i]==1 & cond[i]=="True_24"){ #only congruent and correct trials
      count=count+1
      trial=trial+1
      vec[i]<-Keyrt[i]
    }
    if (Keycorr[i]==0 & cond[i]=="True_24"){ #only congruent and correct trials
      count=count+0
      trial=trial+1
      vec<-vec
    }
    else{
      count=count+0
      vec<-vec
    }
    i=i+1
    Ins_24_meanRT<-mean(vec,na.rm=TRUE)
    assign("Ins_24_RT", z, envir=globalenv())
    assign("Ins_24_corr", count, envir=globalenv())
    assign("Ins_24_meanRT", Ins_24_meanRT , envir=globalenv())
    assign("N_Ins_24", trial,envir=globalenv())
  }
  return (c(Ins_24_corr,Ins_24_meanRT,N_Ins_24))
}




## NOT Inside
#Mean of Melodies, imissing, 0ST separated
count=0
AEFT_miss_0<-function(AEFTtable){
  count=0
  trial=0
  Keyrt<-as.numeric(as.character(AEFTtable$time))
  Keycorr<-as.numeric(as.character(AEFTtable$correct.answer.))
  cond<-as.character(AEFTtable$cond)
  vec<-numeric()
  for (i in 17:144) {#skip practice trials
    if (Keycorr[i]==1 & cond[i]=="False_0"){ #only congruent and correct trials
      count=count+1
      trial=trial+1
      vec[i]<-Keyrt[i]
    }
    if (Keycorr[i]==0 & cond[i]=="False_6"){ #only congruent and correct trials
      count=count+0
      trial=trial+1
      vec<-vec
    }
    else{
      count=count+0
      vec<-vec
    }
    i=i+1
    miss_0_meanRT<-mean(vec,na.rm=TRUE)
    assign("miss_0_RT", z, envir=globalenv())
    assign("miss_0_corr", count, envir=globalenv())
    assign("miss_0_meanRT", miss_0_meanRT, envir=globalenv())
    assign("N_miss_0", trial,envir=globalenv())
  }
  return (c(miss_0_corr,miss_0_meanRT,N_miss_0))
}


#Mean of Melodies, missing, 6ST separated
count=0
AEFT_miss_6<-function(AEFTtable){
  count=0
  trial=0
  Keyrt<-as.numeric(as.character(AEFTtable$time))
  Keycorr<-as.numeric(as.character(AEFTtable$correct.answer.))
  cond<-as.character(AEFTtable$cond)
  vec<-numeric()
  for (i in 17:144) {#skip practice trials
    if (Keycorr[i]==1 & cond[i]=="False_6"){ #only congruent and correct trials
      count=count+1
      trial=trial+1
      vec[i]<-Keyrt[i]
    }
    if (Keycorr[i]==0 & cond[i]=="False_6"){ #only congruent and correct trials
      count=count+0
      trial=trial+1
      vec<-vec
    }
    else{
      count=count+0
      vec<-vec
    }
    i=i+1
    miss_6_meanRT<-mean(vec,na.rm=TRUE)
    assign("miss_6_RT", z, envir=globalenv())
    assign("miss_6_corr", count, envir=globalenv())
    assign("miss_6_meanRT", miss_6_meanRT, envir=globalenv())
    assign("N_miss_6", trial,envir=globalenv())
  }
  return (c(miss_6_corr,miss_6_meanRT,N_miss_6))
}


#Mean of Melodies, missing, 12ST separated
count=0
AEFT_miss_12<-function(AEFTtable){
  count=0
  trial=0
  Keyrt<-as.numeric(as.character(AEFTtable$time))
  Keycorr<-as.numeric(as.character(AEFTtable$correct.answer.))
  cond<-as.character(AEFTtable$cond)
  vec<-numeric()
  for (i in 17:144) {#skip practice trials
    if (Keycorr[i]==1 & cond[i]=="False_12"){ #only congruent and correct trials]
      count=count+1
      trial=trial+1
      vec[i]<-Keyrt[i]
    }
    if (Keycorr[i]==0 & cond[i]=="False_12"){ #only congruent and correct trials
      count=count+0
      trial=trial+1
      vec<-vec
    }
    else{
      count=count+0
      vec<-vec
    }
    i=i+1
    miss_12_meanRT<-mean(vec,na.rm=TRUE)
    assign("miss_12_RT", z, envir=globalenv())
    assign("miss_12_corr", count, envir=globalenv())
    assign("miss_12_meanRT", miss_12_meanRT, envir=globalenv())
    assign("N_miss_12", trial,envir=globalenv())
  }
  return (c(miss_12_corr,miss_12_meanRT,N_miss_12))
}


#Mean of Melodies, missing, 24ST separated

count=0
AEFT_miss_24<-function(AEFTtable){
  count=0
  trial=0
  Keyrt<-as.numeric(as.character(AEFTtable$time))
  Keycorr<-as.numeric(as.character(AEFTtable$correct.answer.))
  cond<-as.character(AEFTtable$cond)
  vec<-numeric()
  for (i in 17:144) {#skip practice trials
    if (Keycorr[i]==1 & cond[i]=="False_24"){ #only congruent and correct trials
      vec[i]<-Keyrt[i]
      count=count+1
      trial=trial+1
    }
    if (Keycorr[i]==0 & cond[i]=="False_24"){ #only congruent and correct trials
      count=count+0
      trial=trial+1
      vec<-vec
    }
    else{
      count=count+0
      vec<-vec
    }
    i=i+1
    miss_24_meanRT<-mean(vec,na.rm=TRUE)
    assign("miss_24_RT", z, envir=globalenv())
    assign("miss_24_corr", count, envir=globalenv())
    assign("miss_24_meanRT",miss_24_meanRT, envir=globalenv())
    assign("N_miss_24", trial,envir=globalenv())
  }
  return (c(miss_24_corr,miss_24_meanRT,N_miss_24))
}


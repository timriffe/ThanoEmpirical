# for Tim, this will choke
if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
  # if I'm on the laptop
  setwd("/home/tim/git/ThanoEmpirical/ThanoEmpirical")
} else {
  # in that case I'm on Berkeley system, and other people in the dept can run this too
  setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/ThanoEmpirical/ThanoEmpirical"))
}

# install.packages("data.table")
# install.packages("RColorBrewer")
library(data.table)
library(reshape2)
library(RColorBrewer)

# simple weighted mean function

wmean <- function(x,w=rep(1,length(x))){
    if (length(x)==0){
        return(NA)
    }
    sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
}

Dat <- local(get(load("Data/Data_long.Rdata")))

Dat <- data.table(Dat)
# a hack, to be removed when weights are properly figured out:
#Dat$wt[is.na(Dat$wt)] <- mean(Dat$wt, na.rm = TRUE)

# for binning purposes, akin to 'completed age'
Dat$tafloor <- floor(Dat$ta)
Dat$cafloor <- floor(Dat$ca)
# I guess actual interview date could be some weeks prior to registered
# interview date? There are two negative thano ages at wave 4 otherwise, but
# still rather close. Likely died shortly after interview.
Dat$tafloor[Dat$tafloor < 0] <- 0

# greater than 15 we lose precision, cut off:
Dat <- Dat[Dat$tafloor <= 15, ]
Dat <- Dat[Dat$cafloor >= 60, ]

# bin ages
Dat$cafloor2 <- Dat$cafloor - Dat$cafloor %% 2
Dat$tafloor2 <- Dat$tafloor - Dat$tafloor %% 2

Dat$cafloor3 <- Dat$cafloor - Dat$cafloor %% 3
Dat$tafloor3 <- Dat$tafloor - Dat$tafloor %% 3

# recode so factors 'back'
#Dat$lim_work <- as.character(Dat$lim_work)
#Dat$back     <- as.character(Dat$back)
#Dat$srh      <- as.character(Dat$srh)
#
#Dat$back <- ifelse(is.na(Dat$back), NA, ifelse(Dat$back == "1. yes",1,0))
#Dat$srh[is.na(Dat$srh)] <- "NA"
#
#recvec        <- c(0:4, NA)
#names(recvec) <- sort(unique(Dat$srh))
#Dat$srh       <- recvec[Dat$srh]
#colnames(Dat) <- gsub("_","",colnames(Dat) )
#Dat <- data.table(Dat)

#varnames      <- c(
#        "iadl3_", "iadl5_", "cesd", "lim_work", "srh", 
#        "bmi", "back",  "hosp", "hosp_stays", "hosp_nights", "nh", 
#        "nh_stays", "nh_nights", "nh_now", 
#        "doc", "doc_visits", "hhc", "meds", "surg", "dent", "shf", "adl_walk", 
#        "adl_dress", "adl_bath", "adl_eat", "adl_bed", "adl_toilet", 
#        "iadl_map", "iadl_tel", "iadl_money", "iadl_meds", "iadl_shop", 
#        "iadl_meals", "mob", "lg_mus", "gross_mot", "fine_mot", "bp", 
#        "diab", "cancer", "lung", "heart", "stroke", "psych", "arth", 
#        "cc", "alc_ev", "alc_days", 
#        "alc_drinks", "smoke_ev", "smoke_cur", "cesd_depr", "cesd_eff", 
#        "cesd_sleep", "cesd_happy", "cesd_lone", "cesd_sad", "cesd_going", 
#        "cesd_enjoy", "med_exp")     


varnames <- c("adl3_", 
  "adl5_", "iadl3_", "iadl5_", "cesd",  "lim_work", "srh", 
  "bmi", "back", "hosp", "hosp_stays", "hosp_nights", "nh", 
  "nh_stays", "nh_nights", "nh_now", "nh_mo", "nh_yr", "nh_days", 
  "doc", "doc_visits", "hhc", "meds", "surg", "dent", "shf", "adl_walk", 
  "adl_dress", "adl_bath", "adl_eat", "adl_bed", "adl_toilet", 
  "iadl_map", "iadl_tel", "iadl_money", "iadl_meds", "iadl_shop", 
  "iadl_meals", "mob", "lg_mus", "gross_mot", "fine_mot", "bp", 
  "diab", "cancer", "lung", "heart", "stroke", "psych", "arth", 
  "cc", "alc_ev", "alc_days", "alc_drinks", "smoke_ev", "smoke_cur", 
  "cesd_depr", "cesd_eff", "cesd_sleep", "cesd_happy", "cesd_lone", 
  "cesd_sad", "cesd_going", "cesd_enjoy", "prob75yo", "alz", "dem", 
  "srm", "pastmem", "ss", "c20b", "name_mo", 
  "name_dmo", "name_yr", "name_dwk", "name_sci", "name_cac", "name_pres", 
  "name_vp", "vocab", "tm", "med_exp", "dwr","twr","iwr",
  "iadl_calc", "mprob", "mprobev", "med_explog")

varnames <- varnames[varnames %in% colnames(Dat)]

# --------------------------------------------------
# This is a sloppy old-school way this
Dat         <- data.frame(Dat)
SurfaceList <- list()
#Dat$mod_freq
for (varname in varnames){
    
    Dati <- Dat[, c("sex","tafloor2","cafloor2","p_wt2",varname)]
    colnames(Dati)[5] <- "V1"
    Mean <- 
            data.table(Dati)[,  list(V1 = wmean(V1,p_wt2)),
                    by = list(sex,tafloor2,cafloor2)]
    Mean <- data.frame(Mean)   
    
    MeanM <- acast(Mean[Mean$sex == "m", ],tafloor2~cafloor2,value.var="V1" )
    MeanF <- acast(Mean[Mean$sex == "f", ],tafloor2~cafloor2,value.var="V1" )
    
    #setnames(Dat,cols="V1",value=varname)
    
    SurfaceList[[varname]] <- list(Male = MeanM,Female = MeanF)
}

# ------------------------------------------------------        
save(SurfaceList, file = "Data/SurfaceList.Rdata")
# ------------------------------------------------------   

#names(SurfaceList)
#SurfaceList[["adl3_"]]
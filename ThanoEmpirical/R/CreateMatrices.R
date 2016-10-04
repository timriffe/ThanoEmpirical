# TR
# This script creates weighted means tabulations for the same study area,
# but NOT by cohort. This was the very very first thing we did as an exploratory
# diagnostic. It was here where we noticed that TTD patterns are strong, but also
# realized that the surface needs to be controlled for birth cohorts. The
# purpose of this script now is just to create a de facto variable list that we
# use to keep everything selected and aligned as we move forward.
# It's a bit of chaff, but no big deal. The next script after this
# is loessSmoothing.R
# -----------------------------------

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

#-----------------------------------
# OPTIONALLY source the pre-processing file
# source("R/PreProcessing.R")
#-----------------------------------

Dat                          <- local(get(load("Data/Data_long.Rdata")))
#-----------------------------------
# DEPRECATE use of imputed dataset
# Dat <- local(get(load("Data/Data_long_imputed.Rdata")))
#-----------------------------------
# greater than 15 we lose precision, cut off:
Dat                          <- Dat[Dat$tafloor <= 15, ]
Dat                          <- Dat[Dat$cafloor >= 60, ]

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
  "iadl_calc", "mprob", "mprobev", "med_explog") # this is bigger than the final list...
# pare down to columns available in current iteration
varnames <- varnames[varnames %in% colnames(Dat)]

# --------------------------------------------------
# This is sloppy old-school 
Dat         <- data.frame(Dat)
SurfaceList <- list()
for (varname in varnames){
    # Note, not controlling for birth cohorts here
    Dati              <- Dat[, c("sex","tafloor","cafloor","p_wt2",varname)]
    colnames(Dati)[5] <- "V1" # this way at least we know what to ask for ;-P
    Mean <-  data.table(Dati)[,  list(V1 = wmean(V1,p_wt2)),
                                 by = list(sex, tafloor, cafloor)]
    Mean  <- data.frame(Mean)   
    
    MeanM <- acast(Mean[Mean$sex == "m", ],tafloor~cafloor,value.var="V1" )
    MeanF <- acast(Mean[Mean$sex == "f", ],tafloor~cafloor,value.var="V1" )
    
    #setnames(Dat,cols="V1",value=varname)
    
    SurfaceList[[varname]] <- list(Male = MeanM, Female = MeanF)
}

# ------------------------------------------------------        
#save(SurfaceList, file = "Data/SurfaceList.Rdata")
# ------------------------------------------------------   


#source("R/SurfMap.R")
#SurfMap(MeanM[,,"1930"], contour=FALSE,outline=FALSE,bg=TRUE)



# ------------------------------------------------------        
save(SurfaceList, file = "Data/SurfaceList.Rdata")

# end
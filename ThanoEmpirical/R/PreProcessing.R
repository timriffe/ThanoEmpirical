
# for Tim, this will choke
if (system("hostname",intern=TRUE)=="triffe-N80Vm"){
  # if I'm on the laptop
  setwd("/home/tim/git/ThanoEmpirical/ThanoEmpirical")
} else {
  # in that case I'm on Berkeley system, and other people in the dept can run this too
  setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/ThanoEmpirical/ThanoEmpirical"))
}
getwd()
# install.packages("lubridate")
library(lubridate)
library(data.table)

# cleaning/processing functions
convertYN <- function(x){
    xx                  <- rep(NA, length(x))
    xx[grepl("yes", x)] <- 1
    xx[grepl("no", x)]  <- 0
    invisible(xx)
}
convertDates <- function(Dat){
    # can't be done with apply because we can't have Date class matrices...
    DateInd       <- grep(pattern="_dt",colnames(Dat))
    for (i in DateInd){
        Dat[,i]    <- as.Date(Dat[,i],origin="1960-1-1")
    }
    invisible(Dat)
}
getThanoAge <- function(Date, DeathDate){
    out <- rep(NA, length(Date))
    Ind <- !is.na(Date)
    out[Ind] <- lubridate::decimal_date(DeathDate[Ind]) - lubridate::decimal_date(Date[Ind])
    out
}
getChronoAge <- function(Date, BirthDate){
    out <- rep(NA, length(Date))
    Ind <- !is.na(Date) & !is.na(BirthDate)
    out[Ind] <- lubridate::decimal_date(Date[Ind]) - lubridate::decimal_date(BirthDate[Ind])
    out
}
imputeWeights <- function(wt,intv_dt){
    if (all(wt == 0)){
        wt2 <- NA * wt
        return(wt2)
    }
    if (sum(wt>0) == 1){
        wt2 <- approx(x = intv_dt[wt>0],
                y = wt[wt>0],
                xout = intv_dt,
                rule = 1:2,
                method = "constant",
                f = .5)$y
    }
    if (sum(wt>0)>=2){
        wt2 <- approx(x = intv_dt[wt>0],
                y = wt[wt>0],
                xout = intv_dt,
                rule = 1:2,
                method = "linear")$y 
    }
    return(wt2)
}

# converts to long format, assumes thano age columns already appended:
#
Dat         <- local(get(load("Data/thanos_long_v2_2.gz")))
dim(Dat)
# remove missed interviews
Dat         <- Dat[!is.na(Dat$intv_dt), ]
nrow(Dat)/ length(unique(Dat$id))
# change all factors to character (to be later recoded in some instances)
Dat[sapply(Dat, is.factor)] <- lapply(Dat[sapply(Dat, is.factor)], as.character)

# make sex column easier to use:
Dat$sex     <- ifelse(as.character(Dat$sex) == "1.male","m","f")

# reduce to deceased-only
Dat         <- Dat[Dat$dead == 1, ]
nrow(Dat)/ length(unique(Dat$id))

# convert dates to native R format
Dat         <- convertDates(Dat)

# merge weights:
Dat$nh_wt[is.na(Dat$nh_wt)] <- 0
Dat$p_wt <- Dat$p_wt + Dat$nh_wt

Dat <- data.table(Dat)
# take care of the rest: 

Dat <- Dat[,p_wt2 := imputeWeights(p_wt,intv_dt), by = list(id) ]

# all zeros removed
# 2341 observations thrown as leading 0s, affecting 934 ids
# 3227 total observations thrown (including all-0s), 1361 total ids affected
Dat <- Dat[!is.na(Dat$p_wt2),]
nrow(Dat)/ length(unique(Dat$id))
# calculate thanatological age
Dat$ta <- getThanoAge(Dat$intv_dt, Dat$d_dt)
Dat$ca <- getChronoAge(Dat$intv_dt, Dat$b_dt)
# there is one individual with an NA b_dt, and NA age,
# but thano age is known

# convert yes/no responses to 1,0
YNcols <- apply(Dat, 2, function(x){
        xx <- unique(x)
        length(xx) <= 3 & any(grepl("yes",xx))
        })
Dat <- data.frame(Dat)
Dat[YNcols] <- lapply(Dat[YNcols], convertYN)

# remove lt, vig, ulc, too inconsistent
Dat$lt        <- NULL
Dat$vig       <- NULL
Dat$ulc       <- NULL
Dat$lt_freq   <- NULL
Dat$mod_freq  <- NULL
Dat$vig_freq  <- NULL

# find suspect columns for false recoding due to factor-> integer direct
wavei <- Dat$wave == 1
DT    <- Dat[!wavei, ]
(Suspects <- colnames(DT)[unlist(lapply(DT,function(x){
                   class(x) == "integer" & all(unique(x) %in% c(1,2,NA))
             }))])
#[1] "cesd_depr"  "cesd_eff"   "cesd_sleep" "cesd_happy" "cesd_lone" 
#[6] "cesd_sad"   "cesd_going" "cesd_enjoy" "iadl_calc" "mprobev"  
# all Suspects check out:

Dat[Suspects] <- lapply(Dat[Suspects], function(x,wavei){
    x[!wavei] <- x[!wavei] - 1
    x
  },wavei=wavei)
   
# now check for possible wave 1 differences
(Suspects <- colnames(Dat)[unlist(lapply(Dat,function(x,wavei){
            all(unique(x[wavei]) %in% c(1,2,3,4,5,NA)) & 
              all(unique(x[!wavei]) %in% c(0,1,NA)) & 
              !all(unique(x[wavei]) %in% c(0,1,NA))
          },wavei=wavei))])
# essentially all cesd measures
Dat[Suspects] <- lapply(Dat[Suspects], function(x){
    x[x == 4] <- 0
    x[x > 1 ]  <- 1
    x
  })

# recode medical expenditure to actual values:

#1=0 to $1,000                 500
#2=~$1000                     1000
#3=$1,001 to 5,000            2500
#4=~$5,000                    5000
#5=$5,001 to $25,000         15000
#6=~$25,000                  25000
#7=$25,001 to $100,000       62500
#8=~$100,000                100000
#9=$100,001 to $500,000     300000
#10=~$500,000               500000
#11=$500,000+              1000000

rec.vec <- c(500,1000,2500,5000,15000,25000,62500,100000,300000,500000,1000000)
names(rec.vec) <- 1:11
Dat$med_exp    <- rec.vec[as.character(Dat$med_exp)]
Dat$med_explog <- log(Dat$med_exp )
# recode self reported health 1 = excellent - 5 = poor
srhrec <- 0:4
names(srhrec) <- sort(unique(Dat$srh))
Dat$srh <- srhrec[Dat$srh] / 4 # now all between 0 and 1. 1 worst.

# Edit: now removed because of waves gap (large)
# light, moderate and vigorous physical activity was coded differently in wave 1 vs other waves...
# now 0 is no activity, and
#activity <- c("lt_freq","mod_freq","vig_freq")
#
#Dat[activity] <- lapply(Dat[activity], function(x,wavei){
#   
#    x[wavei & x == 4 & !is.na(x)]                 <- 4.5
#    x[wavei & x == 3 & !is.na(x)]                 <- 4
#    x[wavei & x == 2 & !is.na(x)]                 <- 3
#    x[wavei & x == 1 & !is.na(x)]                 <- 1.5
#    x[!wavei & x == "1.every day" & !is.na(x)]    <- 1
#    x[!wavei & x == "2.> 1 per week" & !is.na(x)] <- 2
#    x[!wavei & x == "3.1 per week" & !is.na(x)]   <- 3
#    x[!wavei & x == "4.l-3 per mon" & !is.na(x)]  <- 4
#    x[!wavei & x == "5.never" & !is.na(x)]        <- 5
#    as.numeric(x)
#  },wavei=wavei)

# save out, so this doesn't need to be re-run every time
save(Dat,file = "Data/Data_long.Rdata")

#Dat <- local(get(load("Data/Data_long.Rdata")))




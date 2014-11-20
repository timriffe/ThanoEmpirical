
# for Tim, this will choke
if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm","tim-ThinkPad-L440")){
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
convertCI <-  function(x){
  xx                  <- rep(NA, length(x))
  
  xx[x == "1.correct"] <- 0
  xx[x == "2.correct, 1st try"] <- 0
  xx[x == "1.correct, 2nd try"] <- .5
  xx[x == "0.incorrect"]  <- 1
  invisible(as.numeric(xx))
}
convertCESD <- function(x){
  xx <- rep(NA,length(x))
  xx[x == "0.no"]                    <- 0
  xx[x == "4. none or almost none"]  <- 0
  xx[x == "3. some of the time"]     <- .5
  xx[x == "2. most of the time" ]    <- .75
  xx[x ==  "1.yes"]                  <- 1
  xx[x ==  "1. all or almost all"]   <- 1
  xx
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
nrow(Dat)

# remove missed interviews
Dat         <- Dat[!is.na(Dat$intv_dt), ]
nrow(Dat)
nrow(Dat)/ length(unique(Dat$id)) # avg interviews / id
# change all factors to character (to be later recoded in some instances)
#str(Dat)

#Dat[sapply(Dat, is.factor)] <- lapply(Dat[sapply(Dat, is.factor)], as.character)

# make sex column easier to use:
Dat$sex     <- ifelse(Dat$sex == "1.male","m","f")

# reduce to deceased-only
Dat         <- Dat[Dat$dead == 1, ]
nrow(Dat)
nrow(Dat)/ length(unique(Dat$id))

# convert dates to native R format
Dat         <- convertDates(Dat)

# merge weights:
Dat$nh_wt[is.na(Dat$nh_wt)] <- 0
Dat$p_wt <- Dat$p_wt + Dat$nh_wt

Dat <- data.table(Dat)
# take care of the rest: 

Dat <- Dat[,p_wt2 := imputeWeights(p_wt,intv_dt), by = list(id) ]
#Dat$p_wt2[Dat$p_wt==0]
# all zeros removed
# 2341 observations thrown as leading 0s, affecting 934 ids
# 3227 total observations thrown (including all-0s), 1361 total ids affected
Dat <- Dat[!is.na(Dat$p_wt2),]
nrow(Dat)
nrow(Dat)/ length(unique(Dat$id))
# calculate thanatological age
Dat$ta <- getThanoAge(Dat$intv_dt, Dat$d_dt)
Dat$ca <- getChronoAge(Dat$intv_dt, Dat$b_dt)
# there is one individual with an NA b_dt, and NA age,
# but thano age is known

# convert yes/no responses to 1,0
YNcols <- apply(Dat, 2, function(x){
        xx <- unique(x)
        length(xx) <= 4 & any(grepl("yes",xx))
        })
CIcols <- apply(Dat, 2, function(x){
          xx <- unique(x)
          length(xx) <= 5 & any(grepl("correct",xx))
        }) 

colnames(Dat)[YNcols]

colnames(Dat)[CIcols] 

Dat <- data.frame(Dat)
Dat[YNcols] <- lapply(Dat[YNcols], convertYN)
Dat[CIcols] <- lapply(Dat[CIcols], convertCI)
#head(Dat)

# remove lt, vig, ulc, too inconsistent
Dat$lt        <- NULL
Dat$vig       <- NULL
Dat$ulc       <- NULL
Dat$lt_freq   <- NULL
Dat$mod_freq  <- NULL
Dat$vig_freq  <- NULL
Dat$c86b      <- NULL # only in a couple waves
Dat$dem       <- NULL
Dat$alz       <- NULL
Dat$iadl_calc <- NULL
Dat$prob75yo  <- NULL
Dat$nh_mo     <- NULL
Dat$nh_yr     <- NULL
Dat$nh_days   <- NULL

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

rec.vec <- c(500,1000,2500,5000,15000,25000,62500,100000,300000,500000,1000000, NA,NA)
names(rec.vec)      <- c("1 : 0 to 1000-",
  "2 : about 1000",
  "3 : 1001 to 5000-",
  "4 : about 5000",
  "5 : 5001 to 25000-",
  "6 : about 25000",
  "7 : 25001 to 100000-",
  "8 : about 100000",
  "9 : 100001 to 500000-",
  "10: about 500000",
  "11: 500000 above" ,
  "NA" , "")

Dat$med_exp         <- rec.vec[as.character(Dat$med_exp)]
Dat$med_explog      <- log(Dat$med_exp )
# recode self reported health 1 = excellent - 5 = poor
srhrec              <- c(0:4,NA)
names(srhrec)       <- sort(unique(Dat$srh))
Dat$srh             <- srhrec[Dat$srh] / 4 # now all between 0 and 1. 1 worst.
names(srhrec)       <- sort(unique(Dat$srm))
Dat$srm             <- srhrec[Dat$srm] / 4 

# same, worse, better recode:  (1 bad, 0 good)
pastmem             <- c(0:2,NA)
names(pastmem)      <- sort(unique(Dat$pastmem))
Dat$pastmem         <- pastmem[Dat$pastmem] / 2

# do cesd questions (1 bad, 0 good)
cesdquestions       <- colnames(Dat)[grepl("cesd", colnames(Dat))]
cesdquestions       <- cesdquestions[cesdquestions != "cesd"]
Dat[cesdquestions]  <- lapply(Dat[cesdquestions],convertCESD)


# create a single Total Word Recall variables, twr
#"tr20w"(waves(2-10),"tr40w" (waves1-2)
# i.e. 1 is the worst recall, and 0 is the best recall
Dat$tr20w                   <- 1 - Dat$tr20w / 20
Dat$tr40w                   <- 1 - Dat$tr40w / 40

NAind                       <- is.na(Dat$tr20w) & is.na(Dat$tr40w)
BothInd                     <- !is.na(Dat$tr20w) & !is.na(Dat$tr40w)
Dat$tr20w[is.na(Dat$tr20w)] <- 0
Dat$tr40w[is.na(Dat$tr40w)] <- 0
sum(BothInd) == 0 # (otherwise we'd need to divide these by two after adding)
Dat$twr                     <- Dat$tr20w + Dat$tr40w
Dat$twr[NAind]              <- NA

# vocab: 1 worst 0 best
Dat$vocab <- 1 - Dat$vocab / 10

# total mental: 1 worst, 0 best
Dat$tm    <- 1 - Dat$tm / 15

# delayed word recall
Dat$dr20w                   <- 1 - Dat$dr20w / 20
Dat$dr10w                   <- 1 - Dat$dr10w / 10

NAind                       <- is.na(Dat$dr20w) & is.na(Dat$dr10w)
BothInd                     <- !is.na(Dat$dr20w) & !is.na(Dat$dr10w)
Dat$dr20w[is.na(Dat$dr20w)] <- 0
Dat$dr10w[is.na(Dat$dr10w)] <- 0
sum(BothInd) == 0 # (otherwise we'd need to divide these by two after adding)
Dat$dwr                    <- Dat$dr20w + Dat$dr10w
Dat$dwr[NAind]              <- NA

# immediate word recall
Dat$ir20w                   <- 1 - Dat$ir20w / 20
Dat$ir10w                   <- 1 - Dat$ir10w / 10

NAind                       <- is.na(Dat$ir20w) & is.na(Dat$ir10w)
BothInd                     <- !is.na(Dat$ir20w) & !is.na(Dat$ir10w)
Dat$ir20w[is.na(Dat$ir20w)] <- 0
Dat$ir10w[is.na(Dat$ir10w)] <- 0
sum(BothInd) == 0 # (otherwise we'd need to divide these by two after adding)
Dat$iwr                     <- Dat$ir20w + Dat$ir10w
Dat$iwr[NAind]              <- NA

# memory problem:
#[1] ""                                "0. no"                          
#[3] "1. yes"                          "NA"                             
#[5] "4. disp prev record and no cond"
mprob <- c(NA,0,1,0,NA)
names(mprob) <- sort(unique(Dat$mprob))
Dat$mprob <- mprob[Dat$mprob]
# vocab

# scale to fit btwn 0 and 1
rescale <- function(var,Dat,compelment = FALSE){
  Dat[[var]] <- Dat[[var]] / max(Dat[[var]], na.rm = TRUE)
  if (compelment){
    Dat[[var]] <- 1 - Dat[[var]]
  }
  Dat
}

Dat     <- rescale("mob", Dat, FALSE)
Dat     <- rescale("lg_mus", Dat, FALSE) 
Dat     <- rescale("gross_mot", Dat, FALSE)
Dat     <- rescale("fine_mot", Dat, FALSE)

Dat     <- rescale("ss", Dat, TRUE) # complement because more was better in original
Dat     <- rescale("cc", Dat, FALSE)
Dat     <- rescale("alc_days", Dat, FALSE)

Dat     <- rescale("adl3_", Dat, FALSE)
Dat     <- rescale("adl5_", Dat, FALSE)
Dat     <- rescale("iadl3_", Dat, FALSE)
Dat     <- rescale("iadl5_", Dat, FALSE)
Dat     <- rescale("cesd", Dat, FALSE)
#checkwaves <- function(var,Dat){
#  table(Dat[[var]],Dat[["wave"]])
#}
#checkwaves("adl3_",Dat)
#checkwaves("adl5_",Dat)
#checkwaves("iadl3_",Dat)
#checkwaves("iadl5_",Dat)
#checkwaves("cesd",Dat)
# save out, so this doesn't need to be re-run every time
save(Dat,file = "Data/Data_long.Rdata")

#Dat <- local(get(load("Data/Data_long.Rdata")))




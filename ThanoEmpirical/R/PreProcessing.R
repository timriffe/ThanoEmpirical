
# for Tim, this will choke
if (system("hostname",intern=TRUE)=="triffe-N80Vm"){
  # if I'm on the laptop
  setwd("/home/tim/git/ThanoEmpirical/ThanoEmpirical")
} else {
  # in that case I'm on Berkeley system, and other people in the dept can run this too
  setwd(paste0("/hdir/0/",system("whoami",intern=TRUE),"/git/ThanoEmpirical/ThanoEmpirical"))
}

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
Dat         <- local(get(load("Data/thanos_long_v2_1.gz")))

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

# recode self reported health 1 = excellent - 5 = poor
srhrec <- 1:5
names(srhrec) <- sort(unique(Dat$srh))
Dat$srh <- srhrec[Dat$srh]

# vigorous and light physiscal activity:
#vig_freq       lt_freq

# 1: daily
# 2: > 1 /week
# 3: 1 /week
# 4: 1-3 /month
# 5: never
freqrec <- 1:5
names(freqrec) <- sort(unique(Dat$vig_freq  ))
Dat$vig_freq <- freqrec[Dat$vig_freq]
Dat$lt_freq <- freqrec[Dat$lt_freq]
Dat$mod_freq <- freqrec[Dat$mod_freq]
# save out, so this doesn't need to be re-run every time
save(Dat,file = "Data/Data_long.Rdata")


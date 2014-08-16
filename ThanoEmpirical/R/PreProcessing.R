# install.packages("lubridate")
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
# converts to long format, assumes thano age columns already appended:

# setwd needs to change to wherever your parent folder is, assuming the .gz file
# is in a 'Data' folder, and actually has the .gz suffix on it :-)

setwd("/home/triffe/git/ThanoEmpirical/ThanoEmpirical")

# v2_2 has even more variables:
Dat         <- local(get(load("Data/thanos_long_v2_0.gz")))

# change all factors to character (to be later recoded in some instances)
Dat[sapply(Dat, is.factor)] <- lapply(Dat[sapply(Dat, is.factor)], as.character)

# make sex column easier to use:
Dat$sex     <- ifelse(as.character(Dat$sex) == "1.male","m","f")

# reduce to deceased-only
Dat         <- Dat[Dat$dead == 1, ]

# remove missed interviews
Dat         <- Dat[!is.na(Dat$intv_dt), ]

# convert dates to native R format
Dat         <- convertDates(Dat)

# calculate thanatological age
Dat$ta <- getThanoAge(Dat$intv_dt, Dat$d_dt)

# convert yes/no responses to 1,0
YNcols <- apply(Dat, 2, function(x){
        xx <- unique(x)
        length(xx) <= 3 & any(grepl("yes",xx))
        })
Dat[YNcols] <- lapply(Dat[YNcols], convertYN)

# save out, so this doesn't need to be re-run every time
save(Dat,file = "Data/Data_long.Rdata")


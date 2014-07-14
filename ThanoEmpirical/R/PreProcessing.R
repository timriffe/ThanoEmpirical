# install.packages("lubridate")

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
getLong <- function(Dat, 
        vars_ta = c("int_dt","adl3_","adl5_","iadl3_","iadl5_",
                "cesd","cen","div","mar","lim_work",
                "srh","bmi","back","ta_"), 
        vars = c("id","sex","wt","d_yr")){
    # cut down in case some columns not available
    vars_ta <- vars_ta[colSums(sapply(vars_ta,grepl,colnames(Dat))) > 0]
    Vars_ta <- outer(vars_ta,1:10,paste0)
    Dlong   <- list()
    for (i in 1:ncol(Vars_ta)){ # i <- 1
        cols           <- c(vars, Vars_ta[,i])
        colsExtra      <- cols[!cols %in% colnames(Dat)]
        if (length(colsExtra) > 0){
            for (z in colsExtra){
                Dat[[z]] <- NA
            }
        }
        
        Dati           <- Dat[, cols]
        colnames(Dati) <- c(vars, gsub("\\_*\\d*$", "", Vars_ta[,i]))
        Dlong[[i]]     <- Dati
    } 
    Dlong         <- do.call(rbind, Dlong)    
    invisible(Dlong)
}

# setwd needs to change to wherever your parent folder is, assuming the .gz file
# is in a 'Data' folder, and actually has the .gz suffix on it :-)

# setwd("/home/triffe/git/ThanoEmpirical/ThanoEmpirical")
#Dat         <- local(get(load("Data/thanos_wide_v1_0.gz")))

# v1_1 is jumbled somehow, use original smaller file for time being:
Dat         <- local(get(load("Data/thanos_wide_v1_1.gz")))

# make sex column easier to use:
Dat$sex     <- ifelse(as.character(Dat$sex) == "1.male","m","f")
# reduce to deceased-only
Dat         <- Dat[Dat$dead == 1, ]
# convert dates to native R format
Dat         <- convertDates(Dat)

# locate Date columns for calculation of thanatological age:
DInd        <- grep(colnames(Dat),pattern="d_dt")
DateInd     <- grep(colnames(Dat),pattern="int_dt")
TaColnames  <- paste0("ta_",gsub(pattern = "int_dt",replacement = "",colnames(Dat)[DateInd]))
# produce new columns for thano age at each survey wave
for (i in 1:length(DateInd)){
    Dat[[TaColnames[i]]] <- getThanoAge(Dat[,DateInd[i]],Dat[,DInd])
}
# now move to long format
Dat         <- getLong(Dat)

# if a person was absent at a given wave, their ta is NA
Dat         <- Dat[!is.na(Dat$ta), ]

# dim(Dat)
# save out, so this doesn't need to be re-run every time
save(Dat,file = "Data/Data_long.Rdata")



# this script is to check and see how consistent individual weights are over time,
# This is a test to check the feasability / badness of using w5/6 weights for people
# in institutions, who otherwise lack weights. If we can't include people in
# institutions then it will be hard to generalize to the population as there will be
# a major selection problem.
convertDates <- function(Dat){
    # can't be done with apply because we can't have Date class matrices...
    DateInd       <- grep(pattern="_dt",colnames(Dat))
    for (i in DateInd){
        Dat[,i]    <- as.Date(Dat[,i],origin="1960-1-1")
    }
    invisible(Dat)
}
setwd("/home/triffe/git/ThanoEmpirical/ThanoEmpirical")

Dat         <- local(get(load("Data/thanos_long_v2_0.gz")))

# change all factors to character (to be later recoded in some instances)
Dat[sapply(Dat, is.factor)] <- lapply(Dat[sapply(Dat, is.factor)], as.character)

# make sex column easier to use:
Dat$sex     <- ifelse(as.character(Dat$sex) == "1.male","m","f")

# reduce to deceased-only
Dat         <- Dat[Dat$dead == 1, ]

# remove missed interviews
dim(Dat)
Dat         <- Dat[!is.na(Dat$intv_dt), ]
dim(Dat)
# convert dates to native R format
Dat         <- convertDates(Dat)

ids <- unique(Dat$id)
Dat[Dat$id == ids[2],"p_wt"]


#library(data.table)
#Dat <- data.table(Dat)

getslopes <- function(SD){
    SD <- SD[!is.na(SD$p_wt) & SD$p_wt > 0, ]
    p_wt <- SD$p_wt
    intv_dt <- SD$intv_dt
    if (length(p_wt)>=2){
        coefs <- lm(p_wt ~ intv_dt)$coef * c(1,365.25)
        return(c(coefs,length(p_wt))
    } else {
        return(c(NA,NA,NA))
    }
}
#getslopes(SD$p_wt,SD$intv_dt)
#Dat$p_wt    <- as.double(Dat$p_wt)
#Dat$intv_dt <- as.double(Dat$intv_dt)
#Slopes      <- Dat[, list(getslopes(p_wt, intv_dt)), by = list(id)]
Slopes      <- do.call(rbind,lapply(split(Dat,Dat$id),getslopes))

Slopes <- Slopes[!is.na(Slopes[,1]), ]

plot(NULL, type = 'n', xlim = c(0,10),ylim = c(0,60000))
apply(Slopes,1,function(SD){
            abline(a=SD[1],b=SD[2],col=ifelse(sign(SD[2])==1,"#0000FF10","#FF000010"))
            
        })

plot(density(Slopes[,2]))
hist(Slopes,n=500,xlim=c(-10000,10000))

Dat$xreg <- as.integer(Dat$intv_dt)
Dat$yreg <- as.integer(Dat$p_wt)
Dat$yreg[Dat$yreg == 0] <- NA

#install.packages("lme4")
library(lme4)
Dat$xreg <- scale(Dat$xreg )
Dat$yreg <- scale(Dat$yreg )

mod1 <- lmer(yreg ~ xreg + (1 | id), data = Dat)

summary(mod1) # seems to tell me weights on average increase within individuals
# in need of more testing

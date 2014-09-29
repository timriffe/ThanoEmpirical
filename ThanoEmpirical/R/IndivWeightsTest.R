# for Tim, this will choke
if (system("hostname",intern=TRUE)=="triffe-N80Vm"){
  # if I'm on the laptop
  setwd("/home/tim/git/ThanoEmpirical/ThanoEmpirical")
} else {
  # in that case I'm on Berkeley system, and other people in the dept can run this too
  setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/ThanoEmpirical/ThanoEmpirical"))
}



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

#
Dat         <- local(get(load("Data/thanos_long_v2_1.gz")))

# change all factors to character (to be later recoded in some instances)
Dat[sapply(Dat, is.factor)] <- lapply(Dat[sapply(Dat, is.factor)], as.character)

# make sex column easier to use:
Dat$sex     <- ifelse(as.character(Dat$sex) == "1.male","m","f")
#colnames(Dat)[grep("wt",colnames(Dat))]
# reduce to deceased-only
Dat         <- Dat[Dat$dead == 1, ]
#head(Dat)
# remove missed interviews
Dat         <- Dat[!is.na(Dat$intv_dt), ]

dim(Dat)
length(unique(Dat$id))

# convert dates to native R format
Dat         <- convertDates(Dat)

# merge weights:
Dat$nh_wt[is.na(Dat$nh_wt)] <- 0
Dat$p_wt <- Dat$p_wt + Dat$nh_wt

#hist(Dat[Dat$p_wt==0 & Dat$wave %in% c(5,6),"age"]/12)
#table(Dat$sex[Dat$p_wt==0 & Dat$wave %in% c(5,6)])
#wives <- unique(Dat$id[Dat$p_wt==0 & Dat$wave %in% c(5,6)])

# 1) sooo, if an id is all 0s and then positive, then the person was under age
# entered as a spouse, and then became of sufficient age. In these cases, we remove
# observations before reaching age 51.

# 2) if a person has weights, and then these become zero, then it's because 
# they entered an institution not in wave 5 or 6. We carry over the most 
# recent pre-institution weight. (we could see how weights transition from out 
# of home to in-to home in waves 5 and 6 and develop a coefficient, but not yet)

# 3) if a person has all 0 weights, we remove them

# 4) if a person goes from 0 to positive to 0, we cut the first 0s and 
# impute for the last 0s (went from under age, to observed, to institution)



#DatL <- split(Dat, Dat$id)
#

#categories <- unlist(lapply(DatL, function(DD){
#                            wt <- DD$p_wt
#                            if (all(wt > 0)){
#                                return(0)
#                            }
#                            if (all(wt == 0)){
#                                return(1)
#                            }
#                            if (wt[1] == 0 & wt[length(wt)] > 1){
#                                return(2)
#                            }
#                            if (wt[1] > 1 & wt[length(wt)] == 0){
#                                return(3)
#                            }
#                            if (wt[1] == 0 & wt[length(wt)] == 0){
#                                return(4)
#                            }
#                            if (wt[1] > 0 & wt[length(wt)] > 0){
#                                return(5)
#                            }
#                            6 # other complex cases...
#                        }))
#        

# 0     1    2   3      4   5 
# 9360  427  883 1035   51  191 

# 0) 9360 people with simple, all positive weights
# 1) 427  people that never had a positive weight: throw them out
# 2160 people that have had both positive and zero weights
# of which:
# 2) 883  went from 0 to positive (cut leading zeros)
# 3) 1035 went from positive to zero (impute trailing zeros)
# 4) 51   went from zero to positive to zero (cut leading, impute trailing)
# 5) 191  went from positive to zero to positive at least once (impute with mean)

library(data.table)
Dat <- data.table(Dat)
# take care of the rest: 
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
colnames(Dat)
Dat <- Dat[,p_wt2 := imputeWeights(p_wt,intv_dt), by = list(id) ]

# all zeros removed
# 2341 observations thrown as leading 0s, affecting 934 ids
# 3227 total observations thrown (including all-0s), 1361 total ids affected
Dat <- Dat[!is.na(Dat$p_wt2),]

save(Dat, file = "Data/Data_long.Rdata")

#
#checkwaves <- function(wave){
#    any(c(5,6)%in%wave)
#}
#WaveCheck <- Dat[p_wt==0,checkwaves(wave),by=list(id)]
#sum(WaveCheck$V1) / 2932
#
#
#
#getslopes <- function(SD){
#    SD      <- SD[!is.na(SD$p_wt) & SD$p_wt > 0, ]
#    p_wt    <- SD$p_wt
#    intv_dt <- SD$intv_dt
#    if (length(p_wt)>=2){
#        coefs <- lm(p_wt ~ intv_dt)$coef * c(1,365.25)
#        return(c(coefs,length(p_wt)))
#    } else {
#        return(c(NA,NA,NA))
#    }
#}
#
#colnames(Dat)[grep(pattern="wt",colnames(Dat))]
#

#getslopes(SD$p_wt,SD$intv_dt)
#Dat$p_wt    <- as.double(Dat$p_wt)
#Dat$intv_dt <- as.double(Dat$intv_dt)
#Slopes      <- Dat[, list(getslopes(p_wt, intv_dt)), by = list(id)]
#Slopes      <- do.call(rbind,lapply(split(Dat,Dat$id),getslopes))
#
#Slopes <- Slopes[!is.na(Slopes[,1]), ]
#
#plot(NULL, type = 'n', xlim = c(0,10),ylim = c(0,60000))
#apply(Slopes,1,function(SD){
#            abline(a=SD[1],b=SD[2],col=ifelse(sign(SD[2])==1,"#0000FF10","#FF000010"))
#            
#        })
#
#plot(density(Slopes[,2]))
#hist(Slopes,n=500,xlim=c(-10000,10000))
#
#Dat$xreg <- as.integer(Dat$intv_dt)
#Dat$yreg <- as.integer(Dat$p_wt)
#Dat$yreg[Dat$yreg == 0] <- NA
#
##install.packages("lme4")
#library(lme4)
#Dat$xreg <- scale(Dat$xreg )
#Dat$yreg <- scale(Dat$yreg )
#
#mod1 <- lmer(yreg ~ xreg + (1 | id), data = Dat)
#
#summary(mod1) # seems to tell me weights on average increase within individuals
## in need of more testing

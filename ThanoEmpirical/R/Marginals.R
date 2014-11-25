
# just to point out that when looked at along a single age-margin, the same characteristic
# may appear to be a function of years lived and years left, so we must look at a 
# cross-classification in order to determine which is salient.

# there are different ways to do this
# 1) take the crude rate for a given age
# 2) take the marginal of a surface
# * (2) is 'more' purged of artifacts than (1).
# * (1) is however standard.

# for Tim, this will choke
if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
  # if I'm on the laptop
  setwd("/home/tim/git/ThanoEmpirical/ThanoEmpirical")
} else {
  # in that case I'm on Berkeley system, and other people in the dept can run this too
  setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/ThanoEmpirical/ThanoEmpirical"))
}

library(parallel)
library(reshape2)
library(RColorBrewer)
library(data.table)

wmean <- function(x,w=rep(1,length(x))){
  if (length(x)==0){
    return(NA)
  }
  sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
}

Dat <- local(get(load("Data/Data_long.Rdata")))
Dat <- Dat[!is.na(Dat$tafloor) & !is.na(Dat$cafloor), ]
Dat <- Dat[Dat$tafloor <= 15, ]
Dat <- Dat[Dat$cafloor >= 60, ]

SurfaceList <- local(get(load("Data/SurfaceList.Rdata")))

Margins <- mclapply(names(SurfaceList), function(varname, Dat){
    
    Dati <- Dat[, c("sex","tafloor","cafloor","p_wt2",varname)]
    colnames(Dati)[5] <- "V1"
    Meanta <- 
      data.table(Dati)[,  list(V1 = wmean(V1,p_wt2)),
        by = list(sex,tafloor)]
    Meanca <- 
      data.table(Dati)[,  list(V1 = wmean(V1,p_wt2)),
        by = list(sex,cafloor)]
    ta <- acast(Meanta, tafloor~sex, value.var="V1")
    ca <- acast(Meanca, cafloor~sex, value.var="V1")
    list(ta=ta,ca=ca)
  },Dat=Dat,mc.cores=detectCores())

names(Margins) <- names(SurfaceList)

MarginsScaled <- lapply(Margins,function(X){
    list(ta=scale(X$ta),ca=scale(X$ca))
  }
)
names(MarginsScaled) <- names(SurfaceList)
talong <- do.call(rbind, lapply(names(MarginsScaled), 
    function(varname, MarginsScaled){
      ta <- MarginsScaled[[varname]]$ta
      data.frame(Var = varname,ta=as.integer(rownames(ta)),ta)
    },MarginsScaled=MarginsScaled))
calong <- do.call(rbind, lapply(names(MarginsScaled), 
    function(varname, MarginsScaled){
      ca <- MarginsScaled[[varname]]$ca
      data.frame(Var = varname,ca=as.integer(rownames(ca)),ca)
    },MarginsScaled=MarginsScaled))

taM <- acast(talong, ta~Var,value.var = "m")
caM <- acast(calong, ca~Var,value.var = "m")
taF <- acast(talong, ta~Var,value.var = "f")
caF <- acast(calong, ca~Var,value.var = "f")
matplot(taM,type='l', lty = 1, col = "#0000FF50")
matplot(taF,type='l', lty = 1, col = "#FF000050", add =TRUE)


taMslopes <- apply(taM,2,function(y,x=as.integer(rownames(taM))){
    lm(y~x)$coef[["x"]]
  })
taFslopes <- apply(taF,2,function(y,x=as.integer(rownames(taF))){
    lm(y~x)$coef[["x"]]
  })
caMslopes <- apply(caM,2,function(y,x=as.integer(rownames(caM))){
    lm(y~x)$coef[["x"]]
  })
caFslopes <- apply(caF,2,function(y,x=as.integer(rownames(caF))){
    lm(y~x)$coef[["x"]]
  })

# investigate these. Are they actually 
names(taMslopes)[taMslopes > 0]
names(taFslopes)[taMslopes > 0]

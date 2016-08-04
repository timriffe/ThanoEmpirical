
# TR: code deprecated Aug 4, 2016

#if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
#  # if I'm on the laptop
#  setwd("/home/tim/git/ThanoEmpirical/ThanoEmpirical")
#} else {
#  # in that case I'm on Berkeley system, and other people in the dept can run this too
#  setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/ThanoEmpirical/ThanoEmpirical"))
#}
#getwd()
## install.packages("lubridate")


# original variables:
#DatOrig         <- local(get(load("Data/thanos_long_v2_2.gz")))
#DatOrig         <- DatOrig[!is.na(DatOrig$intv_dt), ]
#DatOrig         <- DatOrig[DatOrig$dead == 1, ]
## final variables:
#Dat <- local(get(load("Data/Data_long.Rdata")))
#
## get same columns and rows
#DatOrig <- DatOrig[,colnames(DatOrig) %in% colnames(Dat) ]
#DatOrig <- DatOrig[paste(DatOrig$id, DatOrig$wave) %in% paste(Dat$id, Dat$wave), ]
#
## now get before and after values:
## ------------------------------------------------------        
#LoessList <- local(get(load("Data/LoessList.Rdata")))
## ------------------------------------------------------        
#varnames <- names(LoessList)
## ------------------------------------------------------
#varnames <- varnames[varnames %in% colnames(DatOrig)]
#ListOrig <- lapply(apply(DatOrig[,varnames],2,unique),function(x){
#    x <- x[x!=""]
#    sort(x)
#  })
#ListDat <- apply(Dat[,varnames],2,unique)
#
#lengths <- apply(Dat[,varnames],2,function(x){
#    x <- unique(x)
#    x <- x[!is.na(x)]
#    length(x)
#  })
#ranges <- apply(Dat[,varnames],2,function(x){
#    x <- unique(x)
#    x <- x[!is.na(x)]
#    range(x)
#  })
#varChar <- data.frame(lengths,t(ranges))
#colnames(varChar) <- c("Nvals","lower","upper")
#varChar$binary <- varChar$Nvals == 2
#sum(varChar$binary)
#sum(varChar$upper == 1)
#sum(varChar$upper != 1)
#
#length(ranges)
#Meta <- read.csv( "Data/PercentThano.csv",stringsAsFactors=FALSE)
#Meta$ThermoM <- Meta$ThermoF <- NULL
#
#MakeTable <- function(Group, Meta, tablevars=c("Long","Male","Female")){
#  X <- Meta[Meta$Group == Group, tablevars]
#  # order table rows on value
#  avgThano <- rowMeans(X[,c(2,4)])
#  X        <- X[order(avgThano,decreasing=TRUE),]
#  
#  X[,2] <- paste0("\\% ",  sprintf("%.1f",X[,2]))
#  X[,4] <- paste0("\\% ",  sprintf("%.1f",X[,4]))
#  
#  Y <- cbind(Question = X[,1], 
#    Male = paste(X[,2],X[,3]),
#    Female = paste(X[,4],X[,5]))
#  
#  colnames(Y) <- c("Question",)
#  
#  print(xtable(Y, align = "clrr"),
#    sanitize.colnames.function = function(x){x}, 
#    sanitize.text.function = function(x){x},
#    include.rownames=FALSE)
#}

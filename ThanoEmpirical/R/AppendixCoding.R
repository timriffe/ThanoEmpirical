

if (system("hostname",intern=TRUE)=="triffe-N80Vm"){
  # if I'm on the laptop
  setwd("/home/tim/git/ThanoEmpirical/ThanoEmpirical")
} else {
  # in that case I'm on Berkeley system, and other people in the dept can run this too
  setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/ThanoEmpirical/ThanoEmpirical"))
}
getwd()
# install.packages("lubridate")


# original variables:
DatOrig         <- local(get(load("Data/thanos_long_v2_2.gz")))
DatOrig         <- DatOrig[!is.na(DatOrig$intv_dt), ]
DatOrig         <- DatOrig[DatOrig$dead == 1, ]
# final variables:
Dat <- local(get(load("Data/Data_long.Rdata")))

# get same columns and rows
DatOrig <- DatOrig[,colnames(DatOrig) %in% colnames(Dat) ]
DatOrig <- DatOrig[paste(DatOrig$id, DatOrig$wave) %in% paste(Dat$id, Dat$wave), ]

# now get before and after values:
# ------------------------------------------------------        
LoessList <- local(get(load("Data/LoessList.Rdata")))
# ------------------------------------------------------        
varnames <- names(LoessList)
# ------------------------------------------------------
varnames <- varnames[varnames %in% colnames(DatOrig)]
ListOrig <- lapply(apply(DatOrig[,varnames],2,unique),function(x){
    x <- x[x!=""]
    sort(x)
  })
ListDat <- apply(Dat[,varnames],2,unique)





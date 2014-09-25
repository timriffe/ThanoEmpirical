# for Tim, this will choke
if (system("hostname",intern=TRUE)=="triffe-N80Vm"){
  # if I'm on the laptop
  setwd("/home/tim/git/ThanoEmpirical/ThanoEmpirical")
} else {
  # in that case I'm on Berkeley system, and other people in the dept can run this too
  setwd(paste0("/hdir/0/",system("whoami",intern=TRUE),"/git/ThanoEmpirical/ThanoEmpirical"))
}

library(RColorBrewer)


LoessList <- local(get(load("Data/LoessList.Rdata")))
unlist(lapply(LoessList,function(X){
      class(X[[1]])
    }))

PercentThano <- do.call(rbind,lapply(LoessList, function(X){
      degm     <-  sum( abs(X$Male$Garrows[,"deg"] - 90) * abs(X$Male$Garrows[,"diff"])) / sum(abs(X$Male$Garrows[,"diff"]))
      degf     <-  sum( abs(X$Female$Garrows[,"deg"] - 90) * abs(X$Female$Garrows[,"diff"])) / sum(abs(X$Female$Garrows[,"diff"]))
      c(Male = 100*(1 - degm / 90), Female = 100*(1 - degf / 90))
    }))

hist(PercentThano[,1])
# Blues thano. Reds chrono
FemCol  <- as.character(cut(PercentThano[,2],breaks = seq(0,100,by=20),labels=brewer.pal(5,"RdBu")))
MaleCol <- as.character(cut(PercentThano[,1],breaks = seq(0,100,by=20),labels=brewer.pal(5,"RdBu")))

names(MaleCol) <- names(FemCol) <- rownames(PercentThano)
# create mini pdf files
sapply(rownames(PercentThano), function(x,MaleCol){
    pdf(paste0("Figures/ColorCodes/",x,"_Males.pdf"),height=1,width=1)
    par(mai=c(.05,.05,.05,.05),xaxs = "i", yaxs = "i")
    plot(NULL, type = "n", axes=FALSE,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="")
    rect(0,0,1,1,col=MaleCol[x])
    dev.off()
  },MaleCol=MaleCol)

sapply(rownames(PercentThano), function(x,FemCol){
    pdf(paste0("Figures/ColorCodes/",x,"_Females.pdf"),height=1,width=1)
    par(mai=c(.05,.05,.05,.05),xaxs = "i", yaxs = "i")
    plot(NULL, type = "n", axes=FALSE,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="")
    rect(0,0,1,1,col=FemCol[x])
    dev.off()
  },FemCol=FemCol)


# added long name and group name in spreadsheet, so don't overwrite!
#write.table(round(PercentThano,1),file = "Data/PercentThano.csv",sep=",",col.names = c("Male","Female"),row.names = rownames(PercentThano))

Meta <- read.csv( "Data/PercentThano.csv",stringsAsFactors=FALSE)
table(Meta$Group)


"Figures/ColorCodes/adl_dress_Males.pdf"
library(xtable)









# for Tim, this will choke
if (system("hostname",intern=TRUE)=="triffe-N80Vm"){
  # if I'm on the laptop
  setwd("/home/tim/git/ThanoEmpirical/ThanoEmpirical")
} else {
  # in that case I'm on Berkeley system, and other people in the dept can run this too
  setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/ThanoEmpirical/ThanoEmpirical"))
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
#pdf("Figures/SexDifferences.pdf")
#plot(PercentThano[,1],PercentThano[,2],col=NA,xlab="Male",ylab="Female",asp=1)
#text(PercentThano[,1],PercentThano[,2],rownames(PercentThano),cex=.8)
#abline(a=0,b=1)
#dev.off()
# Blues thano. Reds chrono
FemCol  <- as.character(cut(PercentThano[,2],breaks = seq(0,100,by=10),labels=rev(brewer.pal(10,"RdBu"))))
MaleCol <- as.character(cut(PercentThano[,1],breaks = seq(0,100,by=10),labels=rev(brewer.pal(10,"RdBu"))))

names(MaleCol) <- names(FemCol) <- rownames(PercentThano)
# create mini pdf files
sapply(rownames(PercentThano), function(x,MaleCol){
    this.name <- paste0("Figures/ColorCodes/",x,"_Males.pdf")
    this.name <- gsub(pattern = "_", replacement = "",this.name)
    pdf(this.name,height=1,width=1)
    par(mai=c(.05,.05,.05,.05),xaxs = "i", yaxs = "i")
    plot(NULL, type = "n", axes=FALSE,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="")
    rect(0,0,1,1,col=MaleCol[x])
    dev.off()
  },MaleCol=MaleCol)

sapply(rownames(PercentThano), function(x,FemCol){
    this.name <- paste0("Figures/ColorCodes/",x,"_Females.pdf")
    this.name <- gsub(pattern = "_", replacement = "",this.name)
    pdf(this.name,height=1,width=1)
    par(mai=c(.05,.05,.05,.05),xaxs = "i", yaxs = "i")
    plot(NULL, type = "n", axes=FALSE,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="")
    rect(0,0,1,1,col=FemCol[x])
    dev.off()
  },FemCol=FemCol)


# added long name and group name in spreadsheet, so don't overwrite!
#
#write.table(round(PercentThano,1),file = "Data/PercentThano.csv",sep=",",col.names = c("Male","Female"),
#row.names = rownames(PercentThano))
#write.table(Meta,file = "Data/PercentThano.csv",sep=",",
#        col.names = c("Short",   "Long",    "Male",  
#                "Female",  "Group",   "ThermoM", "ThermoF"),row.names = rownames(Meta))
#colnames(Meta)
#Meta <- read.csv( "Data/PercentThano.csv",stringsAsFactors=FALSE)
#
#ADL <- Meta[Meta$Group == "ADL", ]
#
#
#install.packages("xtable")
#library(xtable)
#ColorCells <- function(x,sex){
#   this.name <- paste0("\\Cell{",x,"_",sex,".pdf}")
#   this.name <- gsub(pattern = "_", replacement = "",this.name)
#   this.name
#  }
#Meta$ThermoM <- ColorCells(Meta$Short,"Males")
#Meta$ThermoF <- ColorCells(Meta$Short,"Females")


Meta <- read.csv( "Data/PercentThano.csv",stringsAsFactors=FALSE)
Meta$Female <- PercentThano[,"Female"]
Meta$Male   <- PercentThano[,"Male"]

write.table(Meta,file = "Data/PercentThano.csv",sep=",",col.names = colnames(Meta),
  row.names = FALSE)

library(xtable)
# Group <- "ADL"
MakeTable <- function(Group, Meta, tablevars=c("Long","Male","ThermoM","Female","ThermoF")){
  X        <- Meta[Meta$Group == Group, tablevars]
  
  # order table rows on value
  avgThano <- rowMeans(X[,c(2,4)])
  X        <- X[order(avgThano,decreasing=TRUE),]
  
  # round nicely
  X[,2] <- paste0("\\% ",  sprintf("%.1f",X[,2]))
  X[,4] <- paste0("\\% ",  sprintf("%.1f",X[,4]))
  
  Y <- cbind(Question = X[,1], 
             Male = paste(X[,2],X[,3]),
             Female = paste(X[,4],X[,5]))
  
  colnames(Y) <- c("Question","Male \\% Thano","Female \\% Thano")
  
  # format and save out to latex table
  print(xtable(Y, align = "cp{6cm}rr"),
    sanitize.colnames.function = function(x){x}, 
    sanitize.text.function = function(x){x},
    include.rownames=FALSE,
    file = file.path("Tables",paste0(Group,".tex")))
}



MakeTable("ADL",Meta)
MakeTable("IADL",Meta)
MakeTable("Chronic",Meta)
MakeTable("Functional",Meta)
MakeTable("Behaviors",Meta)
MakeTable("Psychological",Meta)
MakeTable("Healthcare",Meta)
MakeTable("Cognitive",Meta)
nrow(Meta)
groups <- unique(Meta$Group)

sapply(groups,MakeTable,Meta=Meta)

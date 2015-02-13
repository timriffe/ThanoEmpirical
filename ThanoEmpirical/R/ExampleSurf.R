
# This is for an example surface to show how slopes are calculated.


# for Tim, this will choke
if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
  # if I'm on the laptop
  setwd("/home/tim/git/ThanoEmpirical/ThanoEmpirical")
} else {
  # in that case I'm on Berkeley system, and other people in the dept can run this too
  setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/ThanoEmpirical/ThanoEmpirical"))
}

library(LexisUtils)
library(reshape2)
library(RColorBrewer)

# ------------------------------------------------------        
SurfaceList <- local(get(load("Data/SurfaceList.Rdata")))
# ------------------------------------------------------        
varnames  <- names(SurfaceList)
LoessList <- local(get(load("Data/LoessList.Rdata")))
# ------------------------------------------------------        

varname <- "srh"

source("R/SurfMap.R")
Garrows<- LoessList[[varname]]$Male$Garrows
pdf("Figures/SurfExampleMalesSRH.pdf", width = 10, height = 6)

  SurfMap(LoessList[[varname]]$Male$Surf,napprox=9,contour=FALSE)
  apply(Garrows,1,function(x){
      if (sign(x["diff"]) == 1){
        arrows(x["x1"],x["y1"],x["x2"],x["y2"],col="black",lwd=2)
      } else {
        arrows(x["x2"],x["y2"],x["x1"],x["y1"],col="black",lwd=2)
      }
    })
dev.off()
#  
#  sapply(1:nrow(Garrows), function(X,Garrows){
#      x <- mean(Garrows[X,c("x1","x2")])
#      y <- mean(Garrows[X,c("y1","y2")])
#      text(x,y,X,pos=4)
#    },Garrows=Garrows)
#  sapply(1:nrow(Garrows), function(X,Garrows){
#      x <- mean(Garrows[X,c("x1","x2")])
#      y <- mean(Garrows[X,c("y1","y2")])
#      text(x,y,Garrows[X,"deg"],pos=2)
#    },Garrows=Garrows)
#dev.off()

varname <- "adl3_"
#Coords <- dimnames(SurfaceList[[varname]]$Female)
Coords <- list(as.character(seq(0,14,by=2)),as.character(seq(70,100,by=2)))
pdf("Figures/SurfExampleFemalesADL3_1.pdf", width = 10, height = 6)
#i   <- Meta$Short == varname
Mat <- SurfaceList[[varname]]$Female[Coords[[1]],Coords[[2]]]
Mat[row(Mat) + col(Mat)+70-2 > 100] <- NA
SurfMap(Mat,napprox=9,contour=FALSE)
dev.off()

pdf("Figures/SurfExampleFemalesADL3_2.pdf", width = 10, height = 6)
SurfMap(LoessList[[varname]]$Female$Surf,napprox=9,contour=TRUE)
dev.off()

pdf("Figures/SurfExampleFemalesADL3_3.pdf", width = 10, height = 6)
SurfMap(LoessList[[varname]]$Female$Surf,napprox=9,contour=FALSE)
apply(Garrows,1,function(x){
			if (sign(x["diff"]) == 1){
				arrows(x["x1"],x["y1"],x["x2"],x["y2"],col="black",lwd=2)
			} else {
				arrows(x["x2"],x["y2"],x["x1"],x["y1"],col="black",lwd=2)
			}
		})
dev.off()





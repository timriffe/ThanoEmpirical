

library(parallel)
# for Tim, this will choke
if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm","tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/ThanoEmpirical/ThanoEmpirical")
	Cores <- 1 # laptop overheats...
	if (system("hostname",intern=TRUE) %in% c("tim-ThinkPad-L440")){
		Cores <- 4
	}
} else {
	if (system("hostname",intern=TRUE) == "PC-403478"){
		# on MPIDR PC
		setwd("U://git//ThanoEmpirical//ThanoEmpirical")
		Cores <- detectCores()
	} else {
		# in that case I'm on Berkeley system, and other people in the dept can run this too
		setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/ThanoEmpirical/ThanoEmpirical"))
		Cores <- detectCores()
	}
}

##############################################
# load in results from loessSmoothing.R      #
##############################################
#Results <- local(get(load("Data/LoessQuinquenal_imp.Rdata")))
Results <- local(get(load("Data/LoessQuinquenal.Rdata")))

# make sure no NULL results
NULLS <- unlist(lapply(Results, function(X){
      is.null(X$Male) | is.null(X$Female)
    }))
sum(NULLS) # hopefully 0


# check for errors if sum(NULLS) > 0 to get to bottom of it
#which(unlist(lapply(Results,function(X){
#    class(X$Female) == "try-error"
#  })))

##############################################
# this code block is for produce panel surfaces for
# each variable / cohort / span / sex. Nice diagnostic. The plots
# were not carefully made (axis labels covered, etc), but are useful for 
# flipping through.
##############################################


# global parameters
cellwidths  <- c(1,3,3,3,1)
cellheights <- c(1,2,2,2,2,2)


# a utility function, opens blank device
plotn <- function(xlim = c(0, 1),ylim = c(0,1), mai = c(0, 0, 0, 0)){
  plot(NULL, type = "n", xlim = xlim, ylim = ylim,  axes = FALSE, xlab = "", ylab = "")
}


# creates a single surface, no legend
SurfA <- function(.varname,.sex,.span,.coh,.Results,.ticks){
  grabber <- paste0(.varname,"_",.span)
  A <- .Results[[grabber]][[.sex]]$Surf[,,as.character(.coh)]
  A[A < 0] <- 0 # due to overzealous fit
  
  SurfMap(A, 
    thano = as.integer(rownames(A)), 
    chrono = as.integer(colnames(A)), 
    colramp = colorRampPalette(rev(RColorBrewer::brewer.pal(9, "OrRd")), space = "Lab"), 
    napprox = 10, 
    xlab = "", 
    ylab = "",
    contour = FALSE,
    ticks = .ticks,
    legnd = FALSE,
    outline = FALSE,
    bg=TRUE,
    mai = c(.1,.1,.1,.1))
}


# this is nice, but needs a legend somewhere, or else labeled contours.
# Also standard color scale and breaks.
makePanel <- function(varname,sex,Coh5=c(1905,1910,1915,1920,1925)){
  cellwidths <- c(1,3,3,3,1)
  cellheights <- c(1,2,2,2,2,2)
  par(mai = c(0,0,0,0), xaxs="i",yaxs = "i")
  layout(matrix(c(1,2,3,4,5,6,
        1,7,8,9,10,11,
        12,13,14,15,16,17,
        18,19,20,21,22,23,
        18,24,24,24,24,24),
      ncol=5,nrow=6,byrow=FALSE), width = cellwidths,height=cellheights)
#layout.show()
  
  # labels
  plotn()
  text(.5,.7,paste(varname,sex), cex = 2)
  text(.7,.2,"span=.5", cex = 2)
  plotn()
  text(.2,.6,"1905-\n1909")
  plotn()
  text(.2,.6,"1910-\n1914")
  plotn()
  text(.2,.6,"1915-\n1919")
  plotn()
  text(.2,.6,"1920-\n1924")
  plotn()
  text(.2,.6,"1925-\n1929")
  
  # custom ticks
  ticks <- pretty(Results[[paste0(varname,"_",.5)]][[sex]]$Surf,n=15)
  
  # surfaces for span = .5
  SurfA(varname,sex,.5,Coh5[1],Results,ticks)
  SurfA(varname,sex,.5,Coh5[2],Results,ticks)
  SurfA(varname,sex,.5,Coh5[3],Results,ticks)
  SurfA(varname,sex,.5,Coh5[4],Results,ticks)
  SurfA(varname,sex,.5,Coh5[5],Results,ticks)
  
  # surfaces for span = .7
  plotn()
  text(.5,.2,"span=.7", cex = 2)
  SurfA(varname,sex,.7,Coh5[1],Results,ticks)
  SurfA(varname,sex,.7,Coh5[2],Results,ticks)
  SurfA(varname,sex,.7,Coh5[3],Results,ticks)
  SurfA(varname,sex,.7,Coh5[4],Results,ticks)
  SurfA(varname,sex,.7,Coh5[5],Results,ticks)
  
  # surfaces for span = .9
  plotn()
  text(.5,.2,"span=.9", cex = 2)
  SurfA(varname,sex,.9,Coh5[1],Results,ticks)
  SurfA(varname,sex,.9,Coh5[2],Results,ticks)
  SurfA(varname,sex,.9,Coh5[3],Results,ticks)
  SurfA(varname,sex,.9,Coh5[4],Results,ticks)
  SurfA(varname,sex,.9,Coh5[5],Results,ticks)
  
  plotn()
  text(.5,.5,"legend\ngoes\nhere")
  
}

# this makes a multipage pdf with a surface for each cohort and span for a
# given characteristic

# Females
pdf("Figures/PanelCoh5/Females.pdf",width = sum(cellwidths), height = sum(cellheights))
for (x in varnames){
    makePanel(x,"Female")
  }
dev.off()

# Males
pdf("Figures/PanelCoh5/Males.pdf",width = sum(cellwidths), height = sum(cellheights))
lapply(varnames, function(x){
    makePanel(x,"Male")
  })
dev.off()


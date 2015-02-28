
# for Tim, this will choke
if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/ThanoEmpirical/ThanoEmpirical")
	Cores <- 1 # laptop overheats...
	if (system("hostname",intern=TRUE) %in% c("tim-ThinkPad-L440")){
		Cores <- 4
	}
} else {
	# in that case I'm on Berkeley system, and other people in the dept can run this too
	setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/ThanoEmpirical/ThanoEmpirical"))
	Cores <- detectCores()
}
cat("Working directory:\n",getwd())
source("R/SurfMap.R")


# this script contains just the figures used in the final submission to VYPR.
# many other figures are produced here and there in other R scripts, but are not included.


# this is the figure that looks like a Lexis diagram.
gridcol <- gray(.6) # medium gray, not overwhelming.

pdf("Figures/LexisOrtho.pdf",width=4.5,height=4.5)
par(mai=c(.5,.5,.5,.5),xpd=TRUE)
plot(NULL, type="n",xlim=c(0,110),ylim=c(0,110), axes=FALSE,xlab="",ylab="", asp=1)
segments(rep(0,11),seq(10,111,by=10),seq(10,111,by=10),rep(0,11), col = gridcol)
#segments(111,0,111,1)
segments(0,0,110,0, col = gridcol)
segments(0,0,0,110, col = gridcol)
# horizontal:
segments(rep(0,10),seq(10,100,by=10),seq(100,10,by= -10),seq(10,100,by= 10), col = gridcol)
# vertical
segments(seq(10,100,by=10),rep(0,10),seq(10,100,by=10),seq(100,10,by= -10), col = gridcol)

text(55,-10,"Years lived", cex = .8)
text(-10,55,"Years left",srt=90, cex = .8)

text(0,0,0,pos=1, cex = .8)
text(0,0,0,pos=2, cex = .8)
text(110,0,bquote(omega == 110),cex=.8,pos=1)
text(8,114,bquote(omega == 110),pos=2,cex=.8)
arrows(0,99,99-5,0+5,col="blue",lwd=3)
#arrows(0,55,55-15,0+15,col="blue",lwd=2)
segments(0,71,71,0,col="red",lwd=3)
points(71,0,pch=19,col="red")

text(90,70,"(B) incomplete lifeline", cex = .8)
segments(87,67,70,29,col="blue",lty=2)

text(70,80,"(A) complete lifeline", cex = .8)
segments(67,77,43,29,col="red",lty=2)

polygon(c(72,96,96-13,72),c(0,0,13,13),border = "forestgreen", lwd = 2, lty = 2)
text(90,60,"(C) area studied for\n1915-1919 birth cohort", cex = .8, pos = 4)
segments(97,57,96-13,13,col="forestgreen",lty=2)
dev.off()


#################################################################################

# figure for a mailmail
source("R/SurfMap.R")

# chrono: surgery
grabber <- paste0("mob","_",.7)
Surf <- Results[[grabber]][["Male"]]$Surf[,,as.character(1915)]
# thano:
pdf("Figures/Surf_Male_mob.pdf", width = 10, height = 6)
#dev.new(width = 10, height = 6)
SurfMap(Surf,napprox=9,contour=TRUE,outline=FALSE,bg=TRUE)
dev.off()


# thano:
pdf("Figures/Surf_Male_mob.pdf", width = 10, height = 6)
#dev.new(width = 10, height = 6)
SurfMap(Surf,napprox=9,contour=TRUE,outline=FALSE,bg=TRUE)
dev.off()

grabber <- paste0("back","_",.7)
Surf <- Results[[grabber]][["Female"]]$Surf[,,as.character(1915)]

pdf("Figures/Surf_Female_back.pdf", width = 10, height = 6)
#dev.new(width = 10, height = 6)
SurfMap(Surf,napprox=9,contour=TRUE,outline=FALSE,bg=TRUE)
dev.off()







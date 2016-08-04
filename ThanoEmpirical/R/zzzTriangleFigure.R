# no longer used: these figs were used in early presentations

# for Tim, this will choke
if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm","tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/ThanoEmpirical/ThanoEmpirical")
} else {
	# in that case I'm on Berkeley system, and other people in the dept can run this too
	setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/ThanoEmpirical/ThanoEmpirical"))
}

CurlyBraces <- function(x, y, w, depth = 1, pos = 1, direction = 1,plot,... ) {
  
  a=c(1,2,3,48,50)    # set flexion point for spline
  b=c(0,.2,.28,.7,.8) # set depth for spline flexion point
  
  curve = spline(a, b, n = 50, method = "natural")$y / 2 
  
  curve = c(curve,rev(curve))
  curve = (curve / max(curve)) * depth
 
  a_sequence = rep(x,100)
  b_sequence = seq(y-w/2,y+w/2,length=100)  
  
  # direction
  if(direction==1)
    a_sequence = a_sequence+curve
  if(direction==2)
    a_sequence = a_sequence-curve
  
  # pos
  if (plot){
    if(pos==1)
      lines(a_sequence,b_sequence,... ) # vertical
    if(pos==2)
      lines(b_sequence,a_sequence,... ) # horizontal
  }
  invisible(list(x=a_sequence,y=b_sequence))
}


# study region as section of lifelines
pdf("Figures/Triangle1.pdf",width=4.5,height=4.5)
par(mai=c(.5,.5,.5,.5),xpd=TRUE)
plot(NULL, type="n",xlim=c(0,110),ylim=c(0,110), axes=FALSE,xlab="",ylab="", asp=1)
lines(c(111,rep(110:1,each=2),0),rep(1:111,each=2))
segments(111,0,111,1)
segments(0,0,111,0)
segments(0,0,0,111)
text(55,-10,"Years lived")
text(-10,55,"Years left",srt=90)
text(0,0,0,pos=1)
text(0,0,0,pos=2)
text(110,0,bquote(omega),cex=1.3,pos=1)
text(0,110,bquote(omega),pos=2,cex=1.3)
dev.off()

# study region as section of lifelines
pdf("Figures/Triangle2.pdf",width=4.5,height=4.5)
par(mai=c(.5,.5,.5,.5),xpd=TRUE)
plot(NULL, type="n",xlim=c(0,110),ylim=c(0,110), axes=FALSE,xlab="",ylab="", asp=1)
lines(c(111,rep(110:1,each=2),0),rep(1:111,each=2))
segments(111,0,111,1)
segments(0,0,111,0)
segments(0,0,0,111)
text(55,-10,"Years lived")
text(-10,55,"Years left",srt=90)
text(0,0,0,pos=1)
text(0,0,0,pos=2)
text(110,0,bquote(omega),cex=1.3,pos=1)
text(0,110,bquote(omega),pos=2,cex=1.3)
arrows(0,99,99-5,0+5,col="blue",lwd=2)
arrows(0,55,55-15,0+15,col="blue",lwd=2)
segments(0,70,70,0,col="red",lwd=2)
points(70,0,pch=19,col="red")
dev.off()

# study region as section of lifelines
pdf("Figures/Triangle3.pdf",width=4.5,height=4.5)
par(mai=c(.5,.5,.5,.5),xpd=TRUE)
plot(NULL, type="n",xlim=c(0,110),ylim=c(0,110), axes=FALSE,xlab="",ylab="", asp=1)
lines(c(111,rep(110:1,each=2),0),rep(1:111,each=2))
segments(111,0,111,1)
segments(0,0,111,0)
segments(0,0,0,111)
text(55,-10,"Years lived")
text(-10,55,"Years left",srt=90)
text(0,0,0,pos=1)
text(0,0,0,pos=2)
text(110,0,bquote(omega),cex=1.3,pos=1)
text(0,110,bquote(omega),pos=2,cex=1.3)
polygon(c(70,100,85,70),c(0,0,15,15),border="blue",lwd=2)
dev.off()

####################################################################

# morbidity within a lifeline (similar to Fries)
dev.new(height=1.5,width=4.5)

par(mai=c(.2,.2,.2,.2),xpd=TRUE)
plot(NULL, type="n",xlim=c(0,75),ylim=c(0,1), axes=FALSE,xlab="",ylab="")

polygon(c(50,65,65),c(.2,.2,.8),col = gray(.7), border = NA)
xy <- CurlyBraces(x =.9, y=57.5, w=15, depth = .1, pos = 1, direction = 1,plot = FALSE )
lines(xy$y,xy$x)
xy <- CurlyBraces(x =67, y=.5, w=.6, depth = 2, pos = 2, direction = 1,plot = FALSE )
lines(xy$x,xy$y)
segments(0,.2,65,.2)
points(0,.2,pch=19)
points(65,.2,pch=15)
text(65,.1,"x",xpd=TRUE)

par(mai=c(.2,.2,.2,.2),xpd=TRUE)
plot(NULL, type="n",xlim=c(0,75),ylim=c(0,1), axes=FALSE,xlab="",ylab="")
polygon(c(60,75,75),c(.2,.2,.8),col = gray(.7), border = NA)
segments(0,.2,75,.2)
points(0,.2,pch=19)
points(75,.2,pch=15)
text(75,.1,"x+n",xpd=TRUE)

graphics.off()
plot(0,0,ylim=c(-10,10),xlim=c(-10,10))
CurlyBraces(2, 0, 10, pos = 1, direction = 1 )
CurlyBraces(2, 0, 5,  pos = 1, direction = 2 )
CurlyBraces(1, 0, 10, pos = 2, direction = 1 )
CurlyBraces(1, 0, 5,  pos = 2, direction = 2 )

CurlyBraces(2, 0, 10, pos = 1, direction = 1 )

# study region as section of lifelines
pdf("Figures/TriangleLifeLines.pdf",width=4.5,height=4.5)
par(mai=c(.5,.5,.5,.5),xpd=TRUE)
plot(NULL, type="n",xlim=c(0,110),ylim=c(0,110), axes=FALSE,xlab="",ylab="", asp=1)
lines(c(111,rep(110:1,each=2),0),rep(1:111,each=2))
segments(111,0,111,1)
segments(0,0,111,0)
segments(0,0,0,111)
text(55,-10,"Years lived")
text(-10,55,"Years left",srt=90)
text(0,0,0,pos=1)
text(0,0,0,pos=2)
text(110,0,bquote(omega == 110),cex=1.3,pos=1)
text(5,114,bquote(omega == 110),pos=2,cex=1.3)
arrows(0,99,99-5,0+5,col="blue",lwd=2)
#arrows(0,55,55-15,0+15,col="blue",lwd=2)
segments(0,70,70,0,col="red",lwd=2)
points(70,0,pch=19,col="red")
dev.off()

#
#gridcol <- gray(.6)
## Lexis orthogonal:
#
#pdf("Figures/LexisOrtho.pdf",width=4.5,height=4.5)
#par(mai=c(.5,.5,.5,.5),xpd=TRUE)
#plot(NULL, type="n",xlim=c(0,110),ylim=c(0,110), axes=FALSE,xlab="",ylab="", asp=1)
#segments(rep(0,11),seq(10,111,by=10),seq(10,111,by=10),rep(0,11), col = gridcol)
##segments(111,0,111,1)
#segments(0,0,110,0, col = gridcol)
#segments(0,0,0,110, col = gridcol)
## horizontal:
#segments(rep(0,10),seq(10,100,by=10),seq(100,10,by= -10),seq(10,100,by= 10), col = gridcol)
## vertical
#segments(seq(10,100,by=10),rep(0,10),seq(10,100,by=10),seq(100,10,by= -10), col = gridcol)
#
#text(55,-10,"Years lived", cex = .8)
#text(-10,55,"Years left",srt=90, cex = .8)
#
#text(0,0,0,pos=1, cex = .8)
#text(0,0,0,pos=2, cex = .8)
#text(110,0,bquote(omega == 110),cex=.8,pos=1)
#text(8,114,bquote(omega == 110),pos=2,cex=.8)
#arrows(0,99,99-5,0+5,col="blue",lwd=3)
##arrows(0,55,55-15,0+15,col="blue",lwd=2)
#segments(0,71,71,0,col="red",lwd=3)
#points(71,0,pch=19,col="red")
#
#text(90,70,"(B) incomplete lifeline", cex = .8)
#segments(87,67,70,29,col="blue",lty=2)
#
#text(70,80,"(A) complete lifeline", cex = .8)
#segments(67,77,43,29,col="red",lty=2)
#
#polygon(c(72,96,96-13,72),c(0,0,13,13),border = "forestgreen", lwd = 2, lty = 2)
#text(90,60,"(C) area studied for\n1915-1919 birth cohort", cex = .8, pos = 4)
#segments(97,57,96-13,13,col="forestgreen",lty=2)
#dev.off()
#
##########################################
## a diagram to compare coverage for the different birth cohorts.
#plotn <- function(xlim = c(0,1),ylim = c(0,1), mai = c(0,0,0,0),...){
#  plot(NULL, type = "n", xlim = xlim, ylim = ylim,  axes = FALSE, xlab = "", ylab = "",...)
#}
#
#minmaxcoh <- function(coh1){
#  MaxL <- 2011 - coh1 - 1
#  MinL <- 1992 - (coh1 + 5)
#  c(MinL,MaxL)
#}
#
#minmaxcoh(1905)
#minmaxcoh(1915)
#minmaxcoh(1925)
#
#minmax2polgon <- function(minmax, thano = 12){
#  list(x = c(minmax[1],minmax[2]+1,minmax[2]-thano,minmax[1]),
#    y = c(0,0,thano+1,thano+1))
#}
#graphics.off()
#dev.new(width=4.5,height=3)
#plotn(c(60,110),c(0,15), mai=c(.25,.25,.25,.25), asp = 1)
#polygon(minmax2polgon(minmaxcoh(1905)), lwd = 1, border = "blue", lty = 2)
#polygon(minmax2polgon(minmaxcoh(1915)), lwd = 2, border = "green")
#polygon(minmax2polgon(minmaxcoh(1925)), lwd = 1, border = "red", lty = 3)
#
#minmaxcoh <- function(coh1){
#  MaxL <- 2011 - coh1 - 1
#  MinL <- 1992 - (coh1 + 5)
#  c(MinL,MaxL)
#}
#
#minmaxcoh(1905)
#minmaxcoh(1915)
#minmaxcoh(1925)
#
#minmax2polgon <- function(minmax, thano = 12){
#  list(x = c(minmax[1],minmax[2]+1,minmax[2]-thano,minmax[1]),
#    y = c(0,0,thano+1,thano+1))
#}
#





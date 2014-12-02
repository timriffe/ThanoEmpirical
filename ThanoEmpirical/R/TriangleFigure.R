# for Tim, this will choke
if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm","tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/ThanoEmpirical/ThanoEmpirical")
} else {
	# in that case I'm on Berkeley system, and other people in the dept can run this too
	setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/ThanoEmpirical/ThanoEmpirical"))
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






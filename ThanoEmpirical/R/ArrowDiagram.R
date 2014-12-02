# for Tim, this will choke
if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm","tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/ThanoEmpirical/ThanoEmpirical")
} else {
	# in that case I'm on Berkeley system, and other people in the dept can run this too
	setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/ThanoEmpirical/ThanoEmpirical"))
}

pdf("Figures/ArrowDiagram.pdf")
par(mai=c(.5,.5,.5,.5),xpd=TRUE, xaxs="i",yaxs="i")
plot(NULL, type = "n", xlim = c(-1,1),ylim=c(-1,1), axes=FALSE, ylab="",xlab="")

arrows(rep(0,8),rep(0,8),
		c(1,sin(pi/4),0,-sin(pi/4),-1,-sin(pi/4),0,sin(pi/4)),
		c(0,sin(pi/4),1,sin(pi/4),0,-sin(pi/4),-1,-sin(pi/4)), lwd=2)
text(c(1,sin(pi/4),0,-sin(pi/4),-1,-sin(pi/4),0,sin(pi/4))*1.1,
		c(0,sin(pi/4),1,sin(pi/4),0,-sin(pi/4),-1,-sin(pi/4))*1.1,
		c("0%","50%","100%","50%","0%","50%","100%","50%"),cex=1.3)
dev.off()

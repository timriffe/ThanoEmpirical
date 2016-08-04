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

gridcol <- gray(.6) # medium gray, not overwhelming.

pdf("Figures/Poster/LexisOrtho.pdf",width=4.5,height=4.5)
par(mai=c(.5,.5,.5,.5),xpd=TRUE)
plot(NULL, type="n",xlim=c(0,110),ylim=c(0,110), axes=FALSE,xlab="",ylab="", asp=1)
# light gray up to 100? (the part completed
polygon(c(0,100,0),c(0,0,100), col = gray(.94),border=NA)
segments(rep(0,11),seq(10,111,by=10),seq(10,111,by=10),rep(0,11), col = gridcol)
#segments(111,0,111,1)
segments(0,0,110,0, col = gridcol)
segments(0,0,0,110, col = gridcol)
# horizontal:
segments(rep(0,10),seq(10,100,by=10),seq(100,10,by= -10),seq(10,100,by= 10), col = gridcol)
# vertical
segments(seq(10,100,by=10),rep(0,10),seq(10,100,by=10),seq(100,10,by= -10), col = gridcol)
# dashed line on tri border:
segments(0,100,100,0,lty=2,col=gray(.2))
# ticks:
at <- seq(0,110,by=10)
segments(at,0,at,-1,col=gridcol)
segments(0,at,-1,at,col=gridcol)
text(0,at,at,pos=2,cex=.6)
text(at,0,at,pos=1,cex=.6)
text(c(0,115),c(115,0),c(expression(omega),expression(omega)),cex=.7) # omegas
# say for 1915 cohort?
text(at,-8,at+1915,pos=1,cex=.6)
# halfway through life, line:
segments(0,0,60,60,col=gridcol,lty=2) # 1/2 life
segments(0,0,30,90,col=gridcol,lty=2) # 1/4 life
segments(0,0,90,30,col=gridcol,lty=2) # 3/4 life
text(60,60,expression(frac(1,2)~"way through life"),pos=4,cex=.6)
text(30,90,expression(frac(1,4)),pos=4,cex=.6)
text(90,30,expression(frac(3,4)),pos=4,cex=.6)
# ------------------------------
segments(100,0,100,40,,col=gridcol,lty=2)# this year
text(100,40,"year 2015",cex=.6,pos=3)
segments(100,0,100,40,,col=gridcol,lty=2)
# ------------------------------
text(34,63,"completed lifespans",srt=-45,cex=.6)
text(40,63,"incomplete lifespans",srt=-45,cex=.6)
text(55,-7,"chronological age (years lived) & calendar year", cex = .7)
text(-8,55,"thanatological age (remaining years of life, years left)",srt=90, cex = .7)
segments(0,55,55,0,col="red")        # complete
points(55,0,pch=19,col="red",cex=.7) # complete
text(22,32,"a complete lifeline",srt=-45,cex=.6,pos=3,col="red")
arrows(0,107,100,7,col="blue",length=.07)
text(83,19,"an incomplete lifeline",srt=-45,cex=.6,pos=3,col="blue")
# area studied
polygon(c(72,96,96-13,72),c(0,0,13,13),col = "#A1FF4450", border = "#44FF44",  lwd = 2)
text(70,2,"Area studied for\n1915-1919 cohort", cex = .6, pos = 4)
dev.off()
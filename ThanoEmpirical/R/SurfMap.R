

SurfMap <- function (Surf, 
  thano = as.integer(rownames(Surf)), 
  chrono = as.integer(colnames(Surf)), 
  bp = "OrRd",
  colramp = colorRampPalette(rev(RColorBrewer::brewer.pal(9, bp)), space = "Lab"), 
  napprox = 10, 
 xlab = "Years lived, a", 
 ylab = "Years left, y",
 contour=TRUE) 
{
    ticks <- pretty(Surf,n=napprox)
    zlim  <- range(ticks)
    n     <- length(ticks)-1
    col   <- rev(colramp(n))

    SurfCol <- as.character(cut(Surf,breaks=ticks,labels=col))
    dim(SurfCol) <- dim(Surf)
    
    x <- col(Surf) - 1 + min(chrono)
    y <- row(Surf) - 1 + min(thano)
#	x <- chrono
#	y <- thano
    par(xaxs="i",yaxs="i",xpd=TRUE,mai=c(.5,.5,.5,1.5))
    plot(NULL, type = "n", xlim = c(70,101),ylim=c(0,16),xlab="",ylab="",axes=FALSE,asp=1)
    
    # draw cells
    rect(x,y,x+1,y+1,border=NA,col = SurfCol)
    
    # outline area
    segments(70,0,70,16)
    segments(70,0,101,0)
    segments(70,16,86,16)
    segments(86:100,15:1,87:101,15:1)
    segments(86:101,16:1,86:101,15:0)
    
    # axes
    segments(70,seq(0,15,by=5),69.6,seq(0,15,by=5))
    segments(seq(70,100,by=5),0,seq(70,100,by=5),-.4)
    text(69.6,seq(0,15,by=5),seq(0,15,by=5),pos=2,cex=.8)
    text(seq(70,100,by=5),-.4,seq(70,100,by=5),pos=1,cex=.8)
    
    # legend
    ticksat <- (ticks - min(ticks))/diff(range(ticks)) * 12 + 2.5
    rect(102,ticksat[-length(ticks)],103.5,ticksat[-1],col=col, border = gray(.6))
    text(103.5, ticksat,ticks,cex=.8,pos=4)
    
    # labels
    text(85,-2,xlab)
    text(68,17,ylab,pos=4)
    
    if (contour){
      contour(chrono+.5, thano+.5, t(Surf), add = TRUE, col = gray(.2), levels = ticks, labcex = .8)
    }
}




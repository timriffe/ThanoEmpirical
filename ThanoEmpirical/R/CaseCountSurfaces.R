

# for Tim, this will choke
if (system("hostname",intern=TRUE)=="triffe-N80Vm"){
  # if I'm on the laptop
  setwd("/home/tim/git/ThanoEmpirical/ThanoEmpirical")
} else {
  # in that case I'm on Berkeley system, and other people in the dept can run this too
  setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/ThanoEmpirical/ThanoEmpirical"))
}
library(reshape2)
library(RColorBrewer)

Dat                 <- local(get(load("Data/Data_long.Rdata")))
Dat$ta[Dat$ta < 0]  <- 0
Dat$tafloor         <- floor(Dat$ta)
Dat$cafloor         <- floor(Dat$ca)
# bin ages
Dat$cafloor2        <- Dat$cafloor - Dat$cafloor %% 2
Dat$tafloor2        <- Dat$tafloor - Dat$tafloor %% 2

Dat <- Dat[!is.na(Dat$cafloor) & Dat$cafloor >= 60, ]


#image(t(acast(Dat[Dat$sex=="m",], tafloor2~cafloor2,fun=length)), breaks = c())
#
#seq(0,450,by=50)

Females <- acast(Dat[Dat$sex == "f",], tafloor2~cafloor2, fun = length)
Males   <- acast(Dat[Dat$sex == "m",], tafloor2~cafloor2, fun = length)
dim(Males)
dim(Females)

Females[Females == 0]   <- NA
Males[Males == 0]       <- NA
colpal                  <- colorRampPalette(brewer.pal(9, "Blues"), space = "Lab")

brks            <- seq(0, 450, by = 25)
labcols         <- colpal(length(brks) + 3)[-c(1:2,length(brks) + 1:2)]
Fcolors         <- as.character(cut(Females, 
                                breaks = brks, 
                                labels = colpal(length(brks) + 1)[-c(1:2)]))
Mcolors         <- as.character(cut(Males, 
                                breaks = brks, 
                                labels = colpal(length(brks) + 1)[-c(1:2)]))

Grays           <- colpal(length(brks) + 1)[-c(1:2)]
caf             <- as.integer(colnames(Females))
taf             <- as.integer(rownames(Females))
cam             <- as.integer(colnames(Males))
tam             <- as.integer(rownames(Males))
xf              <- (col(Females) * 2) - 2 + min(caf)
xm              <- (col(Males) * 2) - 2 + min(cam)
yf              <- (row(Females) * 2) - 2 
ym              <- (row(Males) * 2) - 2 
xticks          <- seq(60, 110, by = 5)
yticks          <- seq(0, 20, by = 5)
#graphics.off()
#dev.new(width=7,height=4)
pdf("Figures/CaseCountFemales.pdf",width=7,height=4)
par(mai=c(.25,.6,.25,.25), xaxs = "i", yaxs = "i", xpd = TRUE)
plot(NULL, type = "n", xlim = range(caf)+c(0,2), ylim = range(taf)+c(0,2), 
  axes = FALSE, xlab = "", ylab = "", asp = 1,
  panel.first=list(segments(xticks,0,xticks,-.5),
    segments(60,yticks,60-.5,yticks),
    text(60-.5,yticks,yticks,pos=2,cex=.8),
    text(xticks,-.5,xticks,pos=1,cex=.8),
    text(85,-4,"Years lived"),
    text(56,10,"Years left",srt=90)
    ))
rect(xf,yf,xf+2,yf+2,col=Fcolors,border=NA)
rect(min(caf),min(taf),max(caf)+2,max(taf)+2)
contour(caf+1,taf+1,t(Females),add=TRUE,method="flattest")
# the area where we report on:
segments(70,15,70,0,col="yellow",lwd=3)
segments(85,15,100,0,col="yellow",lwd=3)
segments(70,15,85,15,col="yellow",lwd=3)
text(91,11,"Study Area",srt=-45,cex=1.2)
dev.off()
#dev.new(width=7,height=4)
pdf("Figures/CaseCountMales.pdf",width=7,height=4)
par(mai=c(.25,.6,.25,.25), xaxs = "i", yaxs = "i", xpd = TRUE)
plot(NULL, type = "n", xlim = range(caf), ylim = range(taf),
    axes = FALSE, xlab = "", ylab = "", asp = 1,
    panel.first=list(segments(xticks,0,xticks,-.5),
      segments(60,yticks,60-.5,yticks),
      text(60-.5,yticks,yticks,pos=2,cex=.8),
      text(xticks,-.5,xticks,pos=1,cex=.8),
      text(85,-4,"Years lived"),
      text(56,10,"Years left",srt=90)
    ))
rect(xm,ym,xm+2,ym+2,col=Mcolors,border=NA)
rect(min(cam),min(tam),max(cam)+2,max(tam)+2)
contour(cam+1,tam+1,t(Males),add=TRUE,method="flattest")
# the area where we report on:
segments(70,15,70,0,col="yellow",lwd=3)
segments(85,15,100,0,col="yellow",lwd=3)
segments(70,15,85,15,col="yellow",lwd=3)
text(91,11,"Study Area",srt=-45,cex=1.2)
dev.off()


# --------------------------------------------------------------------
# before and after 1920?
FemalesPre1  <- acast(Dat[Dat$sex == "f" & Dat$b_yr < 1915,], tafloor2~cafloor2, fun = length)
MalesPre1    <- acast(Dat[Dat$sex == "m" & Dat$b_yr < 1915,], tafloor2~cafloor2, fun = length)
FemalesPost1 <- acast(Dat[Dat$sex == "f" & Dat$b_yr >= 1915,], tafloor2~cafloor2, fun = length)
MalesPost1   <- acast(Dat[Dat$sex == "m" & Dat$b_yr >= 1915,], tafloor2~cafloor2, fun = length)

FemalesPre1[FemalesPre1 == 0]     <- NA
MalesPre1[MalesPre1 == 0]         <- NA
FemalesPost1[FemalesPost1 == 0]   <- NA
MalesPost1[MalesPost1 == 0]       <- NA

FemalesPost <- FemalesPre <- Females * NA
MalesPre <- MalesPost     <- Males * NA
FemalesPost[rownames(FemalesPost1),colnames(FemalesPost1)]  <- FemalesPost1
MalesPost[rownames(MalesPost1),colnames(MalesPost1)]        <- MalesPost1
MalesPre[rownames(MalesPre1),colnames(MalesPre1)]           <- MalesPre1
FemalesPre[rownames(FemalesPre1),colnames(FemalesPre1)]     <- FemalesPre1


FcolorsPre         <- as.character(cut(FemalesPre, 
                breaks = brks, 
                labels = colpal(length(brks) + 1)[-c(1:2)]))
McolorsPre         <- as.character(cut(MalesPre, 
                breaks = brks, 
                labels = colpal(length(brks) + 1)[-c(1:2)]))
FcolorsPost         <- as.character(cut(FemalesPost, 
                breaks = brks, 
                labels = colpal(length(brks) + 1)[-c(1:2)]))
McolorsPost         <- as.character(cut(MalesPost, 
                breaks = brks, 
                labels = colpal(length(brks) + 1)[-c(1:2)]))

#
#fields::image.plot(as.integer(colnames(FemalesPre)),as.integer(rownames(FemalesPre)),
#        t(FemalesPre),asp=1,xlim=c(70,110),ylim=c(0,20),zlim=c(0,400),main="pre 1915")
#dev.new()
#fields::image.plot(as.integer(colnames(FemalesPost)),as.integer(rownames(FemalesPost)),
#        t(FemalesPost),asp=1,xlim=c(70,110),ylim=c(0,20),zlim=c(0,400),main="post 1915")


pdf("Figures/CaseCountFemalesPre1915.pdf",width=7,height=4)
par(mai=c(.25,.6,.25,.25), xaxs = "i", yaxs = "i", xpd = TRUE)
plot(NULL, type = "n", xlim = range(caf)+c(0,2), ylim = range(taf)+c(0,2), 
        axes = FALSE, xlab = "", ylab = "", asp = 1,
        panel.first=list(segments(xticks,0,xticks,-.5),
                segments(60,yticks,60-.5,yticks),
                text(60-.5,yticks,yticks,pos=2,cex=.8),
                text(xticks,-.5,xticks,pos=1,cex=.8),
                text(85,-4,"Years lived"),
                text(56,10,"Years left",srt=90)
        ))
rect(xf,yf,xf+2,yf+2,col=FcolorsPre,border=NA)
rect(min(caf),min(taf),max(caf)+2,max(taf)+2)
contour(caf+1,taf+1,t(FemalesPre),add=TRUE,method="flattest")
# the area where we report on:
segments(70,15,70,0,col="yellow",lwd=3)
segments(85,15,100,0,col="yellow",lwd=3)
segments(70,15,85,15,col="yellow",lwd=3)
text(91,11,"Study Area",srt=-45,cex=1.2)
dev.off()
#dev.new(width=7,height=4)
pdf("Figures/CaseCountMalesPre1915.pdf",width=7,height=4)
par(mai=c(.25,.6,.25,.25), xaxs = "i", yaxs = "i", xpd = TRUE)
plot(NULL, type = "n", xlim = range(caf), ylim = range(taf),
        axes = FALSE, xlab = "", ylab = "", asp = 1,
        panel.first=list(segments(xticks,0,xticks,-.5),
                segments(60,yticks,60-.5,yticks),
                text(60-.5,yticks,yticks,pos=2,cex=.8),
                text(xticks,-.5,xticks,pos=1,cex=.8),
                text(85,-4,"Years lived"),
                text(56,10,"Years left",srt=90)
        ))
rect(xm,ym,xm+2,ym+2,col=McolorsPre,border=NA)
rect(min(cam),min(tam),max(cam)+2,max(tam)+2)
contour(cam+1,tam+1,t(MalesPre),add=TRUE,method="flattest")
# the area where we report on:
segments(70,15,70,0,col="yellow",lwd=3)
segments(85,15,100,0,col="yellow",lwd=3)
segments(70,15,85,15,col="yellow",lwd=3)
text(91,11,"Study Area",srt=-45,cex=1.2)
dev.off()


pdf("Figures/CaseCountFemalesPost1915.pdf",width=7,height=4)
par(mai=c(.25,.6,.25,.25), xaxs = "i", yaxs = "i", xpd = TRUE)
plot(NULL, type = "n", xlim = range(caf)+c(0,2), ylim = range(taf)+c(0,2), 
        axes = FALSE, xlab = "", ylab = "", asp = 1,
        panel.first=list(segments(xticks,0,xticks,-.5),
                segments(60,yticks,60-.5,yticks),
                text(60-.5,yticks,yticks,pos=2,cex=.8),
                text(xticks,-.5,xticks,pos=1,cex=.8),
                text(85,-4,"Years lived"),
                text(56,10,"Years left",srt=90)
        ))
rect(xf,yf,xf+2,yf+2,col=FcolorsPost,border=NA)
rect(min(caf),min(taf),max(caf)+2,max(taf)+2)
contour(caf+1,taf+1,t(FemalesPost),add=TRUE,method="flattest")
# the area where we report on:
segments(70,15,70,0,col="yellow",lwd=3)
segments(85,15,100,0,col="yellow",lwd=3)
segments(70,15,85,15,col="yellow",lwd=3)
text(91,11,"Study Area",srt=-45,cex=1.2)
dev.off()
#dev.new(width=7,height=4)
pdf("Figures/CaseCountMalesPost1915.pdf",width=7,height=4)
par(mai=c(.25,.6,.25,.25), xaxs = "i", yaxs = "i", xpd = TRUE)
plot(NULL, type = "n", xlim = range(caf), ylim = range(taf),
        axes = FALSE, xlab = "", ylab = "", asp = 1,
        panel.first=list(segments(xticks,0,xticks,-.5),
                segments(60,yticks,60-.5,yticks),
                text(60-.5,yticks,yticks,pos=2,cex=.8),
                text(xticks,-.5,xticks,pos=1,cex=.8),
                text(85,-4,"Years lived"),
                text(56,10,"Years left",srt=90)
        ))
rect(xm,ym,xm+2,ym+2,col=McolorsPost,border=NA)
rect(min(cam),min(tam),max(cam)+2,max(tam)+2)
contour(cam+1,tam+1,t(MalesPost),add=TRUE,method="flattest")
# the area where we report on:
segments(70,15,70,0,col="yellow",lwd=3)
segments(85,15,100,0,col="yellow",lwd=3)
segments(70,15,85,15,col="yellow",lwd=3)
text(91,11,"Study Area",srt=-45,cex=1.2)
dev.off()



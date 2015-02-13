
# what are some thano by chrono patterns that would produce 
# expansion vs compression of morbidity?

# get a lifetable:
# library(DemogBerkeley)

HMDall <- local(get(load("/home/tim/git/DistributionTTD/DistributionTTD/Data/HMDltper.Rdata")))
head(HMDall)
# imagine 
P <- exp(seq(0,2,length=111)^4)
P <- P / rev(P)[1]
P <- rev(P)

lx <- with(HMDall, lx[Year==2010 & CNTRY == "SWE" & Sex == "f"])
lx <- lx/1e5

plot(0:110,lx,type='l')
lines(0:110,P)

# what would this look like in terms of the survival curve? plot individual cells.
cols <- RColorBrewer::brewer.pal(9,"YlOrRd")
colR <- colorRampPalette(cols,space="Lab")

colvec <- rev(as.character(cut(P,breaks = seq(0,1,by=.01), labels = colR(100))))

plot(NULL, type = "n", xlim=c(0,111),ylim=c(0,1))
lx0 <- c(lx,0)



dx <- -diff(c(lx,0))

devtools::load_all("/home/tim/git/Leaves/PlosOne/R/RiffeetalFunctions")
LX <- Thano(lx,dx,FALSE)
Ind <- col(LX)[,111:1] + row(LX)[111:1, ] > 111

Pmat <- P[row(Ind)]
Pmat[!Ind] <- NA
dim(Pmat) <- dim(Ind) 
ages <- 0:111
#plot(NULL, type = "n", xlim = c(0,111),ylim=c(0,1))
#for (i in 1:111){# lifespans
#	for (a in 1:i) { # years lived
#		rect(ages[a],lx[i],ages[a+1],lx[i+1],border=NA,col = colvec[112-i+a-1])
#	}
#} 
#lines(0:110,lx)
# or the same thing with polygons =  a bit cleaner:
colvec2 <- rev(colvec)
plot(NULL, type = "n", xlim = c(0,111),ylim=c(0,1))
for (i in 1:9){
	polygon(x=c(0:111,112:1)-i+1,y=c(lx0,rev(lx0)),col = colvec2[i],border=FALSE)
}

# a second graphic would drop the color bars to the x axis, stacked vertically from high to low:
plot(NULL, type = "n", xlim = c(0,111),ylim=c(0,.45))
# make dxM
Ncol <- length(unique(colvec))-1
dxM <- matrix(0,nrow=Ncol,ncol=length(dx))
for (i in 1:Ncol){
	dxM[i, 1:(length(dx)-i+1)] <- dx[i:length(dx)]
}
dxM <- rbind(0,dxM)
for (i in 1:9){ # i <- 1
	y1 <- colSums(dxM[1:i,,drop=FALSE])
	y2 <- colSums(dxM[1:(i+1),])
	polygon(x = c(0:110,110:0),y=c(y1,rev(y2)),border=NA,col=colvec2[i])
}
# any characteristic that is 100% present in final year of life will have an age
# dist matching d(x) for the final year of life. Health care expenditure is such an
# example...

# it's important to get an idea of how to interpret P
# 1) easy: it's the proportion of the pop with condition x
# 2) more work: it's the conditional propensity of acquiring condition x
# -- the above stacked plot might not be easily interpretable, need to think on it more.
# although it would work with a binary condition.

# um... if it's the proportion of age x with y remaining years of life (y is the i iterator above)
# then we'd need to multiply the stacked colors by the revelant prop...
# and we could arrive at a single color / number.

# say P is the proportion with i.e., the prevalence. Then we need to multiply each
# stacked layer by the relevant P, and indeed do so for *each* age/y combo, then sum over age.
# Gah!

# again, this is just to make a truer version of the previous graphic: either you got
# it or you don't what is the resulting age pattern?

# make dxM
all(abs(colSums(LX)-rowSums(LX))<1e-12) # LX is symmetrix like so

# Note, I got confused here. Lx is not the item we want to work with,
# but rather dx, the lifespan distribution for a single birth cohort.
# meh.

# this is the unit we ought to stay on, and to compare it with lx, the stable
# age distribution.

# 1) first what is the implied age pattern of characteristic x within a birth cohort?
plot(NULL, type = "n", xlim = c(0,111),ylim=c(0,1))
Pdx <- dx * 0
for (i in 1:111){
	# prevalence in birth cohort at age x...
	Pdx[i] <- sum(P[1:(length(dx)-i+1)] * dx[i:length(dx)])
}
lines(0:110,Pdx,type='l')
lines(0:110,dx,col="red")
lines(0:110,lx,col="blue")
lines(0:110, Pdx / lx, col = "magenta")
lines(0:110, P, col = "green")
text(20,c(.8,.7,.6,.5,.9)-.3,
		c("Livings, l(x)","living with condition P, lp(x)","lifespans, d(x)","prop livings with P, lp(x)/l(x)", "prop with char P by years left P(y)"),
		pos=4,col=c("blue","black","red","magenta","green"))
# close to hazard:
#lines(0:110,dx/lx,col="black")

# the green and pink lines are key here. If it's 100% a thano process it can still look
# like a chrono process rather convincingly.

mx <- with(HMDall, mx[Year==2010 & CNTRY == "SWE" & Sex == "f"])
# make function to produce last plot and take P and lx as inputs.
thanochrono <- function(mx,Py){
	lx <- c(1,exp(-cumsum(mx)))[1:111]
	dx <- -diff(c(lx,0))
	Pdx <- dx * 0
	for (i in 1:111){
		# prevalence in birth cohort at age x...
		Pdx[i] <- sum(Py[1:(length(dx)-i+1)] * dx[i:length(dx)])
	}
	cbind(mx=mx,lx=lx,dx=dx,Pdx=Pdx,Py=Py)
}

makeP <- function(h,e){
	P <- exp(seq(0,h,length=111)^e)
	P <- P / rev(P)[1]
	rev(P)
}
matplot(0:110,thanochrono(mx,makeP(2,4)),type='l',lty=1)
matplot(0:110,thanochrono(mx*.5,makeP(2,4)),type='l',lty=2,add=TRUE)

matplot(0:110,thanochrono(mx,makeP(2,4)),type='l',lty=1)
matplot(0:110,thanochrono(mx,makeP(3,2)),type='l',lty=2,add=TRUE)

A <- thanochrono(mx,makeP(2,4))
B <- thanochrono(mx,makeP(3,2))
sum(B[,"Pdx"]*.5:110.5)/sum(B[,"Pdx"]) # less concentration = lower mean age of condition P
sum(A[,"Pdx"]*.5:110.5)/sum(A[,"Pdx"]) # more thano concentration = higher mean age of condition P
plot(0:110,cumsum(A[,"Pdx"])/sum(A[,"Pdx"]),type='l') # more thano concentration = cum age dist closer to lx = more compressed
lines(0:110,cumsum(B[,"Pdx"])/sum(B[,"Pdx"]),col="blue")
abline(h=c(.25,.75))
# now this corresponds with Thano!

LXP <- LX * Pmat

plot(0:110,rowSums(LXP, na.rm=TRUE),type = 'l')
lines(0:110,P,col="blue",lty=2)
lines(0:110,colSums(LXP, na.rm=TRUE)/lx, col = "red")
image(t(LX/LXP))
LXP <- LX*P
LXPC <- LX*(1-P)


image(log(t(Pmat)), asp = TRUE, main = "True imposed thano pattern (log proportion)")
image(log(t(LXP)))

par(mfrow=c(1,2))
plot(0:110,colSums(LXP) / colSums(LXP + LXPC),type='l',col="blue")
lines(0:110,rowSums(LXP) / rowSums(LXP + LXPC),col="red")
legend("topright",lty=1,col=c("blue","red"),
        legend=c("apparent chrono pattern","actual thano pattern"),
        bty="n")

plot(0:110,lx,type='l',col="blue")
polygon(c(0:110,110:0),c(lx,rev(colSums(LXPC))),col="red")
lines(0:110,colSums(LXP))
mx2lx <- function(mx){
    N <- length(mx)
    c(1,exp(-cumsum(mx)))[-(N+1)]
}
lx2dx <- function(lx){
    -diff(c(lx,0))
}


CompressMorbidity <- function(mx, P){
    lx   <- mx2lx(mx)
    LX   <- Thano(lx,lx2dx(lx))
    
    LXP  <- LX*P
    LXPC <- LX*(1-P)
    list(LXP=LXP,LXPC=LXPC)
}
mx <-  SWE$mx[SWE$Year==1990]
P <- exp(seq(0,2,length=111)^3)
P <- P / rev(P)[1]
P <- rev(P)
#plot(0:110,P,type='l',xlim=c(0,5))
Morb1 <- CompressMorbidity(mx,P)
Morb2 <- CompressMorbidity(mx*.7,P)
sum(mx2lx(mx*.8))
plot(0:110,colSums(Morb1$LXP), type='l',lwd=2)
lines(0:110,colSums(Morb2$LXP), col="red",lwd=2)

# conclusion, if all morbidity happens in the last year of life, then the
# morbidity distribution approaches the deaths distribution and morbidity compresses
# at the same pace as mortality.
# If morbidity is 100% thanatological, but is gradual

Morb1 <- CompressMorbidity(mx,P)
Morb2 <- CompressMorbidity(c(rep(0,80),rep(2,31)),P)
sum(mx2lx(mx*.8))
plot(0:110,colSums(Morb1$LXP), type='l',lwd=2)
lines(0:110,colSums(Morb2$LXP), col="red",lwd=2)

x<-0:110
aa <- .0001
ba <-.05
(Pa <- cumsum(aa * exp(ba*x)))

ay <- 1
by <- -.6
(Py <- ay * exp(by*sqrt(x)))
plot(x,Pa,type='l',log='y')
lines(x,Py)
plot(0:4,Py[1:5])
Pya <- outer(Py,Pa,"+")
Pya <- Pya / max(Pya) 
Pya[row(Pya) + col(Pya) - 2 > 110] <- 0
MorbidityGompertz <- function(mx, a=0:110, aa=1e-4, ba=.05, y=0:110, ay=1, by=-.6){
    lx    <- mx2lx(mx)
    Lya   <- Thano(lx,lx2dx(lx))
    Pa    <- cumsum(aa * exp(ba*a))
    Py    <- ay * exp(by*sqrt(y))
    Pya   <- outer(Py,Pa,"+")
    Pya   <- Pya / max(Pya) 
    LXP   <- Lya*P
    LXPC  <- Lya*(1-P)
    list(Lya=Lya,Pya=Pya,LXP=LXP,LXPC=LXPC)
}

filled.contour(x,x,log(Pya),xlab="Thano",ylab="Chrono")

Morb1 <- MorbidityGompertz(mx)
Morb2 <- MorbidityGompertz(mx*.7)
sum(mx2lx(mx*.8))

plot(0:110,colSums(Morb1$LXP)/colSums(Morb1$Lya), type='l',lwd=2)
lines(0:110,colSums(Morb2$LXP)/colSums(Morb2$Lya), col="red",lwd=2)

morbCond <- function(Morb){
    colSums(Morb$LXP)
}
morbIncond <- function(Morb){
    colSums(Morb$LXP)/colSums(Morb$Lya)
}
plot(0:110,morbIncond(Morb1), type='l',lwd=2)
lines(0:110,morbIncond(Morb2), col="red",lwd=2)


plot(cos(seq(0,pi/2,length=111))^3)

library(RColorBrewer)
display.brewer.all()

# what are some thano by chrono patterns that would produce 
# expansion vs compression of morbidity?

# get a lifetable:
library(DemogBerkeley)

SWE <- readHMDweb("SWE","fltper_1x1",password=pw,username=us)
head(SWE)

# imagine 
P <- exp(seq(0,2,length=111)^4)
P <- P / rev(P)[1]
P <- rev(P)

lx <- SWE$lx[SWE$Year==2010]
lx <- lx/1e5

plot(0:110,lx,type='l')
lines(0:110,P)

dx <- -diff(c(lx,0))

devtools::load_all("/home/tim/git/Leaves/PlosOne/R/RiffeetalFunctions")

LX <- Thano(lx,dx)

LXP <- LX*P
LXPC <- LX*(1-P)

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


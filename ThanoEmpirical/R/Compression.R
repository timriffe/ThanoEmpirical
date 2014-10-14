
# what are some thano by chrono patterns that would produce 
# expansion vs compression of morbidity?

# get a lifetable:
library(DemogBerkeley)

SWE <- readHMDweb("SWE","fltper_1x1",password=pw,username=us)
head(SWE)

# imagine 
P <- exp(seq(0,2,length=111)^3)
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
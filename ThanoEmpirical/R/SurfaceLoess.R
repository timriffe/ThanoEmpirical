setwd("/home/tim/git/ThanoEmpirical/ThanoEmpirical")
library(LexisUtils)
Dat <- local(get(load("Data/Data_long.Rdata")))

# some globals
t.age <- 0:15
c.age <- 65:90
newdata <- expand.grid(ta=t.age,ca=c.age)

mod <- loess('srh~ta+ca' ,data = Dat, weights = p_wt2, span = .5)

FindMaxGradientMatrix <- function(loess.mod, t.age = 0:15, c.age = 65:90){
    newdata        <- expand.grid(ta = t.age, ca = c.age)
    Surf           <- predict(mod, newdata)
    dimnames(Surf) <- list(t.age,c.age)
    
    # some origins to searh around...
    t.origin <- seq(2.5,12.5,by=5)
    c.origin <- seq(67.5,87.5,by=5)
    origins  <- expand.grid(ta=t.origin,ca=c.origin)
    
    radius   <- 2
    
    radii  <- seq(0,2*pi,length=201)[1:200]
    x.circ <- cos(radii) * radius
    y.circ <- sin(radii) * radius
    # i <- 1
    Garrows <- matrix(nrow = nrow(origins), 
            ncol = 5,
            dimnames = list(NULL, c("y1","x1","y2","x2","diff")))
    for (i in 1:nrow(origins)){
        newdatai <- data.frame(ta = origins$ta[i]+y.circ,
                               ca = origins$ca[i]+x.circ)
        predvec  <- predict(mod, newdatai)
        MaxG     <- which.max(abs(predvec[1:100] - predvec[101:200]))
        Garrows[i, ] <- c(unlist(newdatai[MaxG, ]),
                          unlist(newdatai[MaxG + 100, ]),  
                          predvec[MaxG + 100] - predvec[MaxG])
    }
    Garrows
}

Garrows <- FindMaxGradientMatrix(mod)

DirGrad <- sign(Garrows[,"diff"])
Dir     <- mean(Garrows[,"y2"] - Garrows[,"y1"]) / mean(Garrows[,"x2"] - Garrows[,"x1"])
Rise    <- mean(Garrows[,"diff"]) / 4



Surf           <- predict(mod, newdata)
dimnames(Surf) <- list(t.age,c.age)
summary(mod)
LexisMap(Surf,log=FALSE,
        xlab = "Years Lived", 
        ylab = "Years Left",
#        main = paste("Males",varname),
        contour = TRUE, 
        LexRef = FALSE)





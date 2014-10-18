# for Tim, this will choke
if (system("hostname",intern=TRUE)=="triffe-N80Vm"){
  # if I'm on the laptop
  setwd("/home/tim/git/ThanoEmpirical/ThanoEmpirical")
  Cores <- 1 # laptop overheats...
} else {
  # in that case I'm on Berkeley system, and other people in the dept can run this too
  setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/ThanoEmpirical/ThanoEmpirical"))
  Cores <- detectCores()
}
cat("Working directory:\n",getwd())

library(LexisUtils)
library(parallel)
Dat <- local(get(load("Data/Data_long.Rdata")))
SurfaceList <- local(get(load("Data/SurfaceList.Rdata")))



# fixed study area:
# now between chrono ages 70 and 100, below thano age 15, and where chrono + thano <= 100.
# something like:
# 15    |----\ 85
# |     |     \
# 0  70 |______\ 100

# ------------------------------------------------------        

#varname <- "lt_freq";sex<-"m"; t.age =  0:15; c.age = 70:100; MaxL = 100; span = .5; radius = 2
FindMaxGradientMatrix <- function(varname, 
        Dat, 
        sex,
        t.age = 0:15,
        c.age = 70:100,
        MaxL = 100,          # max completed lifespan
        span = .5, 
        radius = 2,
        N = 200){
    mod            <- loess(paste0(varname,'~ta+ca') ,
                            data = Dat[Dat$sex == sex, ], 
                            weights = p_wt2, 
                            span = span
                    )
             
    newdata        <- expand.grid(ta = t.age, ca = c.age)
    # easier to keep dimensions straight if we predict over rectangular grid, 
    # then throw out values outside range
    newdata        <- newdata + .5
    Surf           <- predict(mod, newdata)
    Surf           <- matrix(Surf, 
                             ncol = length(c.age),
                             dimnames = list(floor(t.age), 
                                             floor(c.age)
                                             )
                            )

    Surf[! col(Surf) - 1 + min(c.age) + row(Surf) - 1 + min(t.age) <= MaxL] <- NA
    # some origins to search around...
    t.origin <- seq(2.5, 12.5, by = 5)
    c.origin <- seq(72.5, 97.5, by = 5)
    origins  <- expand.grid(ta = t.origin, ca = c.origin)
    origins  <- origins[rowSums(origins) <= MaxL,]

    radii  <- seq(0,2*pi,length=(N+1))[1:N]
    x.circ <- cos(radii) * radius
    y.circ <- sin(radii) * radius
    # i <- 1
    Garrows <- matrix(nrow = nrow(origins), 
            ncol = 6,
            dimnames = list(NULL, c("y1","x1","y2","x2","diff","deg")))
    for (i in 1:nrow(origins)){
        newdatai <- data.frame(ta = origins$ta[i]+y.circ,
                               ca = origins$ca[i]+x.circ)
        predvec  <- predict(mod, newdatai)
        Diffs    <- abs(predvec[1:(N/2)] - predvec[((N/2)+1):N])
        if (any(is.na(Diffs))){
          Garrows[i, ] <- rep(NA,6)
        } else {
          MaxG     <- which.max(Diffs)
          Garrows[i, ] <- c(unlist(newdatai[MaxG, ]),
            unlist(newdatai[MaxG + (N/2), ]),  
            predvec[MaxG + (N/2)] - predvec[MaxG],
            radii[MaxG]*180/pi)
        }
       
    }
    list(Garrows = Garrows, Surf = Surf, span = span, sex = sex, varname = varname)
}


varnames <- names(SurfaceList) # sex <- "m"
# these appear to break on the origin search thing, make more robust.

LoessList  <- mclapply(varnames, function(varname,Dat){
            cat(varname,"Male\n")
            Male <- try(FindMaxGradientMatrix(varname, Dat, "m", N = 1000))
            cat(varname,"Female\n")
            Female <- try(FindMaxGradientMatrix(varname, Dat, "f", N = 1000))
            
            list(Male = Male,
                 Female = Female)
        }, Dat = Dat, mc.cores = Cores) # careful to change this!
 
   
#Error <- varnames[unlist(lapply(lapply(LoessList,"[[",1),class))=="try-error"]
#
#varname <- "srh" 

names(LoessList) <- unlist(lapply(LoessList, function(x){x$Male$varname}))
#LoessList[[Error]] <- NULL
save(LoessList,file="Data/LoessList.Rdata")


# -----------------------------------------
#colnames(Dat)
#
#Dat$Coh1915 <- ifelse(Dat$b_yr < 1915, 0,1)
#Dat$Coh1920 <- ifelse(Dat$b_yr < 1920, 0,1)
#Dat$Coh1925 <- ifelse(Dat$b_yr < 1925, 0,1)
#varname <- "srh"
#sex<-"m";span<-.5
#mod            <- loess(paste0(varname,'~ta+ca+Coh1920') ,
#        data = Dat[Dat$sex == sex, ], 
#        weights = p_wt2, 
#        span = 1
#)
#
#newdata        <- expand.grid(ta = t.age, ca = c.age,Coh1920 = c(0,1))
## easier to keep dimensions straight if we predict over rectangular grid, 
## then throw out values outside range
#newdata[,1:2]        <- newdata[,1:2] + .5
#Surf           <- predict(mod, newdata)
#
#Surf1           <- matrix(Surf[,,1], 
#        ncol = length(c.age),
#        dimnames = list(floor(t.age), 
#                floor(c.age)
#        )
#)
#Surf2           <- matrix(Surf[,,2], 
#        ncol = length(c.age),
#        dimnames = list(floor(t.age), 
#                floor(c.age)
#        )
#)
#
#
#NAcells <- function(Surf, MaxL, c.age, t.age, MaxL=100){
#    Surf[! col(Surf) - 1 + min(c.age) + row(Surf) - 1 + min(t.age) <= MaxL] <- NA
#    Surf
#}
#Surf1 <- NAcells(Surf1, MaxL, c.age, t.age)
#Surf2 <- NAcells(Surf2, MaxL, c.age, t.age)
#
#SurfMap(Surf1)
#dev.new()
#SurfMap(Surf2)
#
#source("R/SurfMap.R")


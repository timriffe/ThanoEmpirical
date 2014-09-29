# for Tim, this will choke
if (system("hostname",intern=TRUE)=="triffe-N80Vm"){
  # if I'm on the laptop
  setwd("/home/tim/git/ThanoEmpirical/ThanoEmpirical")
} else {
  # in that case I'm on Berkeley system, and other people in the dept can run this too
  setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/ThanoEmpirical/ThanoEmpirical"))
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
        radius = 2){
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

    radii  <- seq(0,2*pi,length=201)[1:200]
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
        MaxG     <- which.max(abs(predvec[1:100] - predvec[101:200]))
        Garrows[i, ] <- c(unlist(newdatai[MaxG, ]),
                          unlist(newdatai[MaxG + 100, ]),  
                          predvec[MaxG + 100] - predvec[MaxG],
                          radii[MaxG]*180/pi)
    }
    list(Garrows = Garrows, Surf = Surf, span = span, sex = sex, varname = varname)
}


varnames <- names(SurfaceList)
# these appear to break on the origin search thing, make more robust.
LoessList  <- mclapply(varnames, function(varname,Dat){
            cat(varname,"Male\n")
            Male <- try(FindMaxGradientMatrix(varname, Dat, "m"))
            cat(varname,"Female\n")
            Female <- try(FindMaxGradientMatrix(varname, Dat, "f"))
            
            list(Male = Male,
                 Female = Female)
        }, Dat = Dat, mc.cores = 8) # careful to change this!
      

Error <- varnames[unlist(lapply(lapply(LoessList,"[[",1),class))=="try-error"]


names(LoessList) <- varnames
LoessList[[Error]] <- NULL
save(LoessList,file="Data/LoessList.Rdata")










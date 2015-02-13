
# Try splitting the data into two birth cohorts and again into two periods 
# and see if there are any essential differences that would hint at/against
# compression
library(parallel)
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

library(LexisUtils)

Dat <- local(get(load("Data/Data_long.Rdata")))
SurfaceList <- local(get(load("Data/SurfaceList.Rdata")))


Dat    <- Dat[Dat$age >= 65, ]
Dat    <- Dat[!is.na(Dat$b_yr), ]

# imagine some large birth cohorts:

# what ages can they have obtained in border years:

# 5-year cohorts instead: (each row is a different cohort here)
Cohorts <- outer(seq(1900,1930,by=5), 0:4, "+")
#Cohorts <- outer(seq(1900,1930,by=10), 0:9, "+")

#nrow(Dat[Dat$sex=="f",])/nrow(Dat)
#length(unique(Dat$id[Dat$sex=="f"]))/length(unique(Dat$id))

# fixed study area:
# now between chrono ages 70 and 100, below thano age 15, and where chrono + thano <= 100.
# something like:
# 15    |----\ 85
# |     |     \
# 0  70 |______\ 100

# ------------------------------------------------------        
# varname <- "srh"
#varname <- "lt_freq";sex<-"m"; t.age =  0:15; c.age = 70:100; MaxL = 100; span = .5; 
FitLoess <- function(varname, 
		Dat, 
		sex,
		t.age = 0:15,
		c.age = 70:100,
		    # max completed lifespan
		span = .5,
		Cohorts){
	Surfs <- list()
	for (i in 1:nrow(Cohorts)){ # i <- 1
		Ci <- Cohorts[i, ]
		# conservative here to cut tails
		maxL <- min(2011 - min(Ci) - 2,100)
		minL <- max(1992 - max(Ci) + 2, 70)
		mod            <- loess(paste0(varname,'~ta+ca') ,
								data = Dat[Dat$sex == sex & Dat$b_yr %in% Ci, ], 
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
		
		#maxL <- as.integer(rev(colnames(Surf)[colSums(Surf,na.rm=TRUE)!=0])[1])
		#MaxL <- min (maxL, MaxL)
		Surf[! col(Surf) - 1 + min(c.age) + row(Surf) - 1 + min(t.age) <= maxL] <- NA
		Surf[! col(Surf) - 1 + min(c.age) + row(Surf) - 1 + min(t.age) >= minL] <- NA
		
		# TODO: need to also NA-out areas where loess needs to extrapolate to...
		
		Surfs[[i]] <- Surf
	}
	
	list(Surf = Surfs, span = span, sex = sex, varname = varname, C = Cohorts)
}

#par(mfrow=c(7,1), mai = c(.1,.1,.1,.1), xaxs="i",yaxs = "i")
#for (i in 1:7){
#	image(c.age,t.age,t(Surfs[[i]]),asp=1,ylim=c(0,15),xlim=c(70,100),zlim=c(.25,.8))
#	text(100,8,min(Cohorts[i,]),pos=4)
#	text(100,6,max(Cohorts[i,]),pos=4)
#}
#range(unlist(Surfs),na.rm=TRUE)


varnames <- names(SurfaceList) # sex <- "m"
# these appear to break on the origin search thing, make more robust.

LoessList  <- mclapply(varnames, function(varname,Dat){
			cat(varname,"Male\n")
			Male <- try(FitLoess(varname, Dat, "m", Cohorts=Cohorts))
			cat(varname,"Female\n")
			Female <- try(FitLoess(varname, Dat, "f", Cohorts=Cohorts))
			
			list(Male = Male,
					Female = Female)
		}, Dat = Dat, mc.cores = Cores) # careful to change this!


#Error <- varnames[unlist(lapply(lapply(LoessList,"[[",1),class))=="try-error"]
#
#varname <- "srh" 

names(LoessList) <- unlist(lapply(LoessList, function(x){x$Male$varname}))
#LoessList[[Error]] <- NULL
save(LoessList,file="Data/LoessListCohrts5.Rdata")



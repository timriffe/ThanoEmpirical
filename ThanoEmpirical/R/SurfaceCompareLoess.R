

library(parallel)
# for Tim, this will choke
if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm","tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/ThanoEmpirical/ThanoEmpirical")
	Cores <- 1 # laptop overheats...
	if (system("hostname",intern=TRUE) %in% c("tim-ThinkPad-L440")){
		Cores <- 4
	}
} else {
	if (system("hostname",intern=TRUE) == "PC-403478"){
		# on MPIDR PC
		setwd("U://git//ThanoEmpirical//ThanoEmpirical")
		Cores <- detectCores()
	} else {
		# in that case I'm on Berkeley system, and other people in the dept can run this too
		setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/ThanoEmpirical/ThanoEmpirical"))
		Cores <- detectCores()
	}
}
getwd()
if (! "LexisUtils" %in% rownames(installed.packages())){
	devtools::install_github("timriffe/LexisUtils/LexisUtils")
}

library(LexisUtils)
source("R/SurfMap.R")




Dat           <- local(get(load("Data/Data_long_imputed.Rdata")))
SurfaceList   <- local(get(load("Data/SurfaceList.Rdata")))
varnames      <- names(SurfaceList)



Dat      <- Dat[Dat$age >= 65, ]
Dat      <- Dat[!is.na(Dat$b_yr), ]
Dat$Coh5 <- Dat$b_yr -  Dat$b_yr %% 5 
Coh5keep <- c(1900, 1905, 1910, 1915, 1920, 1925, 1930)
Coh5     <- c(1905, 1910, 1915, 1920, 1925) # i.e. we use the preceding and subsequent cohorts for help fitting
Dat      <- Dat[Dat$Coh5 %in% Coh5keep, ]

all(varnames %in% colnames(Dat))

#nrow(Dat)
#length(unique(Dat$id))
#nrow(Dat[Dat$Coh5 == 1915,])
#length(unique(Dat$id[Dat$Coh5 == 1915]))







# first take, block off
#runthis <- FALSE
#if (runthis){
# imagine some large birth cohorts:

# what ages can they have obtained in border years:

# 5-year cohorts instead: (each row is a different cohort here)
#Cohorts <- outer(seq(1900,1930,by=5), 0:4, "+")
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
#FitLoess <- function(varname, 
#		Dat, 
#		sex,
#		t.age = 0:15,
#		c.age = 70:100,
#		    # max completed lifespan
#		span = .5,
#		Cohorts){
#	Surfs <- list()
#	for (i in 1:nrow(Cohorts)){ # i <- 1
#		Ci <- Cohorts[i, ]
#		# conservative here to cut tails
#		maxL <- min(2011 - min(Ci) - 2,100)
#		minL <- max(1992 - max(Ci) + 2, 70)
#		mod            <- loess(paste0(varname,'~ta+ca') ,
#								data = Dat[Dat$sex == sex & Dat$b_yr %in% Ci, ], 
#								weights = p_wt2, 
#								span = span
#							)
#		newdata        <- expand.grid(ta = t.age, ca = c.age)
#		# easier to keep dimensions straight if we predict over rectangular grid, 
#		# then throw out values outside range
#		newdata        <- newdata + .5
#		Surf           <- predict(mod, newdata)
#		Surf           <- matrix(Surf, 
#				ncol = length(c.age),
#				dimnames = list(floor(t.age), 
#						floor(c.age)
#				)
#		)
#		
#		#maxL <- as.integer(rev(colnames(Surf)[colSums(Surf,na.rm=TRUE)!=0])[1])
#		#MaxL <- min (maxL, MaxL)
#		Surf[! col(Surf) - 1 + min(c.age) + row(Surf) - 1 + min(t.age) <= maxL] <- NA
#		Surf[! col(Surf) - 1 + min(c.age) + row(Surf) - 1 + min(t.age) >= minL] <- NA
#		
#		# TODO: need to also NA-out areas where loess needs to extrapolate to...
#		
#		Surfs[[i]] <- Surf
#	}
#	
#	list(Surf = Surfs, span = span, sex = sex, varname = varname, C = Cohorts)
#}

#par(mfrow=c(7,1), mai = c(.1,.1,.1,.1), xaxs="i",yaxs = "i")
#for (i in 1:7){
#	image(c.age,t.age,t(Surfs[[i]]),asp=1,ylim=c(0,15),xlim=c(70,100),zlim=c(.25,.8))
#	text(100,8,min(Cohorts[i,]),pos=4)
#	text(100,6,max(Cohorts[i,]),pos=4)
#}
#range(unlist(Surfs),na.rm=TRUE)


# sex <- "m"
# these appear to break on the origin search thing, make more robust.

#LoessList  <- mclapply(varnames, function(varname,Dat){
#			cat(varname,"Male\n")
#			Male <- try(FitLoess(varname, Dat, "m", Cohorts=Cohorts))
#			cat(varname,"Female\n")
#			Female <- try(FitLoess(varname, Dat, "f", Cohorts=Cohorts))
#			
#			list(Male = Male,
#					Female = Female)
#		}, Dat = Dat, mc.cores = Cores) # careful to change this!
#

#Error <- varnames[unlist(lapply(lapply(LoessList,"[[",1),class))=="try-error"]
#
#varname <- "srh" 

#names(LoessList) <- unlist(lapply(LoessList, function(x){x$Male$varname}))
##LoessList[[Error]] <- NULL
#save(LoessList,file="Data/LoessListCohrts5.Rdata")
#
#library(rgl)
#}
#library(scatterplot3d)

#
#sexi <- Dat$sex == "m" & !is.na(Dat$color)
#Dat$color <- cutramp(Dat$mob)
#nai <- is.na(Dat$color)
#Dat$color <- paste0(cutramp(Dat$mob))
#Dat$color[nai] <- NA
#scatterplot3d(x=Dat$ca[sexi],y=Dat$ta[sexi],z=Dat$b_yr[sexi], color = Dat$color[sexi],pch=19)

# change the cohort range to view different slices:
#cohi <- Dat$b_yr %in% 1920:1925
#rgl::plot3d(x=jitter(Dat$ca[sexi & cohi]),y=jitter(Dat$ta[sexi& cohi]),z=jitter(Dat$b_yr[sexi& cohi]),
#  col = Dat$color[sexi& cohi],aspect=TRUE,size=1, type='s',alpha=0.3)

#cutramp <- function(x,n=10,colramp = BrewerRamp("YlOrRd")){
#
#  rg <- range(pretty(x))
#  breaks <- seq(rg[1],rg[2],length.out=n+1)
#  as.character(cut(x,breaks=breaks,labels =colramp(n) ))
#}

# varname <- "back"; sex <- "f";span = 0.5;.Coh5 <- Coh5; t.age = 0:12; c.age = 70:100


FitLoess <- function(varname, 
  Dat, 
  sex,
  t.age = 0:12,    # some 5-year cohorts simply don't have 15 years, cut it lower
  c.age = 70:100,  # standard matrix size, though we may NA certain unobserved cells
  span = .5, # will vary
  .Coh5){
    # conservative here to cut tails
    maxL  <- 100
    minL  <- 70

    # multiplicative give the most freedom.
    mod   <- loess(paste0(varname,'~Coh5 * ta * ca') ,
                    data = Dat[Dat$sex == sex, ], 
                    weights = p_wt2, # this, plus point density both act as weights
                    span = span,     # a variable passed in, or smoothness
                    # is similiar conceptually to a 1:1:1 aspect ratio. Everything is in years...
                    normalize = FALSE,
                    control = loess.control(trace.hat="approximate")
    )
    
    newdata        <- expand.grid(ta = t.age+.5, ca = c.age+.5, Coh5 = .Coh5)
    # easier to keep dimensions straight if we predict over rectangular grid, 
    # then throw out values outside range
    #newdata        <- newdata + .5
    Surf           <- predict(mod, newdata)
 
    dimnames(Surf) <- list(floor(t.age),floor(c.age), .Coh5)

    # need to trim on the left side where applicable, since some questions didn't enter until
    # wave 2 or 3. There are many such cases, so check and make sure we dont' extrapolate.
  # sex <- "m"
  # varname <- "cesd"
    MissingWaves <- tapply(Dat[Dat$sex==sex,varname],Dat[Dat$sex==sex,"wave"], function(x){
        all(is.na(x))
      })
    LeftYear <- 1992
    RightYear <- 2011
    if (any(MissingWaves)){
      Waves <- which(MissingWaves)
      # if it's 1 or two, trim, if it's more, then return NULL for now. Be conservative.
      if (any(Waves %in% 3:9)){
        return(NULL) # takes care of gaps. Also takes care of late missing waves.
      }
      if (any(Waves %in% c(1,2))){
        WaveGetYr <- max(Waves[Waves < 3]) + 1
        LeftYear  <- round(mean(as.numeric(format(Dat$intv_dt[Dat$wave == WaveGetYr], "%Y"))))
      }
      if (any(Waves == 10)){
        RightYear  <- round(mean(as.numeric(format(Dat$intv_dt[Dat$wave == 9], "%Y"))))
      }

    }
    # this reduces extrapolation outside of data points 
    for (i in 1:dim(Surf)[3]){
      #maxL  <- 2011 - Coh5[i] - 1
      #maxt  <- tamax[as.character(Coh5[i])]
      #keept <- as.integer(rownames(Surf)) <= maxt
      A     <- Surf[,,i]
      MaxL <- RightYear - .Coh5[i] - 1
      A[ col(A) - 1 + 70 + row(A) - 1 > MaxL] <- NA
# possibly need to trim lower left corner too: dimnames(A)
      MinL <- LeftYear - (.Coh5[i] + 5)
      A[col(A) + 70 - 1 < MinL] <- NA
      #A[!keept, ] <- NA 
      Surf[,,i] <- A
    }
  list(Surf = Surf, span = span, sex = sex, varname = varname, Cohorts = Coh5)
}



allcombos     <- expand.grid(varname = varnames, span = c(.5,.7,.9))
# divide into list for lapply
allcomboswide <- as.data.frame(t(allcombos),stringsAsFactors = FALSE)
#LoessList  <- mclapply(varnames, function(varname,Dat){
#			cat(varname,"Male\n")
#			Male <- try(FitLoess(varname, Dat, "m", Cohorts=Cohorts))
#			cat(varname,"Female\n")
#			Female <- try(FitLoess(varname, Dat, "f", Cohorts=Cohorts))
#			
#			list(Male = Male,
#					Female = Female)
#		}, Dat = Dat, mc.cores = Cores)

do.this <- FALSE
# do.this <- TRUE
if(do.this){

	# catch so that Windows doesn't try...
	if (!Sys.info()[['sysname']] == 'Windows'){
Results <- mclapply(allcomboswide, function(x,Dat,.Coh5.){
    cat(x[1],"Female\n")
    Female <- try(FitLoess(varname = x[1], 
      Dat = Dat, 
      sex = "f",
      t.age = 0:12,    # some 5-year cohorts simply don't have 15 years, cut it lower
      c.age = 70:100,  # standard matrix size, though we may NA certain unobserved cells
      span =  as.numeric(x[2]), # will vary
      .Coh5 = Coh5))
    cat(x[1],"Male\n")
    Male <- try(FitLoess(varname = x[1], 
      Dat = Dat, 
      sex = "m",
      t.age = 0:12,    # some 5-year cohorts simply don't have 15 years, cut it lower
      c.age = 70:100,  # standard matrix size, though we may NA certain unobserved cells
      span =  as.numeric(x[2]), # will vary
      .Coh5 = .Coh5.))
    list(Male = Male, Female = Female)
  }, Dat = Dat, .Coh5. = Coh5, mc.cores = detectCores())


} else {
	# to make mclapply() work on Windows (a hack is required, will warn)
		# x <- allcomboswide[[1]]
	source("http://www.stat.cmu.edu/~nmv/setup/mclapply.hack.R")
	Results <- mclapply(allcomboswide, function(x,Dat,.Coh5.){
				cat(x[1],"Female\n")
				Female <- try(FitLoess(varname = x[1], 
								Dat = Dat, 
								sex = "f",
								t.age = 0:12,    # some 5-year cohorts simply don't have 15 years, cut it lower
								c.age = 70:100,  # standard matrix size, though we may NA certain unobserved cells
								span =  as.numeric(x[2]), # will vary
								.Coh5 = Coh5))
				cat(x[1],"Male\n")
				Male <- try(FitLoess(varname = x[1], 
								Dat = Dat, 
								sex = "m",
								t.age = 0:12,    # some 5-year cohorts simply don't have 15 years, cut it lower
								c.age = 70:100,  # standard matrix size, though we may NA certain unobserved cells
								span =  as.numeric(x[2]), # will vary
								.Coh5 = .Coh5.))
				list(Male = Male, Female = Female)
			}, Dat = Dat, .Coh5. = Coh5)
	
	
}

names(Results) <- unlist(lapply(Results, function(X){
					paste0(X$Male$varname,"_", X$Male$span)
				}))
save(Results,file="Data/LoessQuinquenal_imp.Rdata")
}
# --------------------------------------------------------
# start correlation / exploration work here:


#Results <- local(get(load("Data/LoessQuinquenal_imp.Rdata")))
Results <- local(get(load("Data/LoessQuinquenal.Rdata")))



NULLS <- unlist(lapply(Results, function(X){
      is.null(X$Male) | is.null(X$Female)
    }))
sum(NULLS)
#which(unlist(lapply(Results,function(X){
#    class(X$Female) == "try-error"
#  })))

# now check for NULL values, which will have been produced if we have questions
# introduced later than wave 3, or with missing waves thereafter. We can just throw these out
# for the time being
##############################################
# this code block is for produce panel surfaces for
# each variable / cohort / span / sex. Nice diagnostic. The plots
# were not carefully made (axis labels covered, etc), but are useful for understanding.
##############################################

cellwidths <- c(1,3,3,3,1)
cellheights <- c(1,2,2,2,2,2)
plotn <- function(xlim = c(0,1),ylim = c(0,1), mai = c(0,0,0,0)){
  plot(NULL, type = "n", xlim = xlim, ylim = ylim,  axes = FALSE, xlab = "", ylab = "")
}




#.varname <- "adl3_"
#.sex <- "Female"
#.span <- .5
#.coh <- 1905
#.Results <- Results
#.ticks <- ticks
#names(Results)
SurfA <- function(.varname,.sex,.span,.coh,.Results,.ticks){
  grabber <- paste0(.varname,"_",.span)
  A <- .Results[[grabber]][[.sex]]$Surf[,,as.character(.coh)]
  A[A < 0] <- 0 # due to overzealous fit
  
  # cuttting now taken care of in loess fitting
#  MaxL <- 2011 - .coh - 1
#  A[! col(A) - 1 + 70 + row(A) - 1 < MaxL] <- NA
#  # possibly need to trim lower left corner too: dimnames(A)
#  MinL <- 1992 - (.coh + 5)
#  A[col(A) + 70 - 1 <= MinL] <- NA
  SurfMap(A, 
    thano = as.integer(rownames(A)), 
    chrono = as.integer(colnames(A)), 
    colramp = colorRampPalette(rev(RColorBrewer::brewer.pal(9, "OrRd")), space = "Lab"), 
    napprox = 10, 
    xlab = "", 
    ylab = "",
    contour = FALSE,
    ticks = .ticks,
    legnd = FALSE,
    outline = FALSE,
    bg=TRUE,
    mai = c(.1,.1,.1,.1))
}

# sex <- "Female"
makePanel <- function(varname,sex,Coh5=c(1905,1910,1915,1920,1925)){
  cellwidths <- c(1,3,3,3,1)
  cellheights <- c(1,2,2,2,2,2)
  par(mai = c(0,0,0,0), xaxs="i",yaxs = "i")
  layout(matrix(c(1,2,3,4,5,6,
        1,7,8,9,10,11,
        12,13,14,15,16,17,
        18,19,20,21,22,23,
        18,24,24,24,24,24),
      ncol=5,nrow=6,byrow=FALSE), width = cellwidths,height=cellheights)
#layout.show()
#
#plot(runif(10),runif(10))
#plot(runif(10),runif(10))
#plot(runif(10),runif(10))
  
  
  plotn()
  text(.5,.7,paste(varname,sex), cex = 2)
  text(.7,.2,"span=.5", cex = 2)
  plotn()
  text(.2,.6,"1905-\n1909")
  plotn()
  text(.2,.6,"1910-\n1914")
  plotn()
  text(.2,.6,"1915-\n1919")
  plotn()
  text(.2,.6,"1920-\n1924")
  plotn()
  text(.2,.6,"1925-\n1929")
  
  ticks <- pretty(Results[[paste0(varname,"_",.5)]][[sex]]$Surf,n=15)
  
  SurfA(varname,sex,.5,Coh5[1],Results,ticks)
  SurfA(varname,sex,.5,Coh5[2],Results,ticks)
  SurfA(varname,sex,.5,Coh5[3],Results,ticks)
  SurfA(varname,sex,.5,Coh5[4],Results,ticks)
  SurfA(varname,sex,.5,Coh5[5],Results,ticks)
  
  plotn()
  text(.5,.2,"span=.7", cex = 2)
  SurfA(varname,sex,.7,Coh5[1],Results,ticks)
  SurfA(varname,sex,.7,Coh5[2],Results,ticks)
  SurfA(varname,sex,.7,Coh5[3],Results,ticks)
  SurfA(varname,sex,.7,Coh5[4],Results,ticks)
  SurfA(varname,sex,.7,Coh5[5],Results,ticks)
  
  plotn()
  text(.5,.2,"span=.9", cex = 2)
  SurfA(varname,sex,.9,Coh5[1],Results,ticks)
  SurfA(varname,sex,.9,Coh5[2],Results,ticks)
  SurfA(varname,sex,.9,Coh5[3],Results,ticks)
  SurfA(varname,sex,.9,Coh5[4],Results,ticks)
  SurfA(varname,sex,.9,Coh5[5],Results,ticks)
  
  plotn()
  text(.5,.5,"legend\ngoes\nhere")
  
}

#graphics.off()
#dev.new(width = sum(cellwidths), height = sum(cellheights))
# varname<- "adl3_"

pdf("Figures/PanelCoh5/Females.pdf",width = sum(cellwidths), height = sum(cellheights))
for (x in varnames){
    makePanel(x,"Female")
  }
dev.off()



pdf("Figures/PanelCoh5/Males.pdf",width = sum(cellwidths), height = sum(cellheights))
lapply(varnames, function(x){
    makePanel(x,"Male")
  })
dev.off()

###########################################################









#Maler <- do.call(rbind,lapply(varnames, function(x, Results){
#    grabber <- paste0(x,"_",.7)
#    A <- Results[[grabber]][["Male"]]$Surf[,,"1915"]
#    get_r(A)
#  }, Results = Results))
#rownames(Maler) <- varnames
#
#Femaler <- do.call(rbind,lapply(varnames, function(x, Results){
#      grabber <- paste0(x,"_",.7)
#      A <- Results[[grabber]][["Female"]]$Surf[,,"1915"]
#      get_r(A)
#    }, Results = Results))
#rownames(Femaler) <- varnames

# --------------------------------------
# this was how I decided which points to mark in the plots:
mark.points <- FALSE
if (mark.points){
plot(Maler[,1],Maler[,2],type="n")
text(Maler[,1],Maler[,2],varnames,cex=.8)
TextM1 <- identify(Maler[,1],Maler[,2],n=13)

plot(Femaler[,1],Femaler[,2],type="n")
text(Femaler[,1],Femaler[,2],varnames,cex=.8)
TextF1 <- identify(Femaler[,1],Femaler[,2],n=13)


plot(Maler[,3],Maler[,4],type="n")
text(Maler[,3],Maler[,4],varnames,cex=.8)
TextM2 <- identify(Maler[,3],Maler[,4],n=13)


plot(Femaler[,3],Femaler[,4],type="n")
text(Femaler[,3],Femaler[,4],varnames,cex=.8)
TextF2 <- identify(Femaler[,3],Femaler[,4],n=13)
}

# saved output using dput(), needed for below plotting
TextM1 <- c(7L, 9L, 21L, 22L, 47L, 52L, 53L, 54L, 56L, 60L, 62L, 63L, 72L
)
TextM2 <- c(9L, 21L, 22L, 40L, 41L, 43L, 47L, 52L, 54L, 60L, 61L, 62L, 
  63L)
TextF1 <- c(7L, 9L, 21L, 41L, 43L, 47L, 52L, 53L, 55L, 56L, 59L, 60L, 63L
)
TextF2 <- c(21L, 22L, 34L, 41L, 43L, 45L, 47L, 52L, 56L, 59L, 60L, 63L, 
  74L)

# the stats in the paper( > .8)
colSums(Maler > .8)
colSums(Femaler > .8)

# the figures in the paper:
pdf("Figures/MaleCorr1.pdf")
par(mai=c(1,1,.5,.5))
plot(Maler[,1],Maler[,2],type="n",xlim=c(0,1),ylim=c(0,1),
  xaxs="i",yaxs="i",cex.axis=1.2,cex.lab=1.2,xlab = "chronological correlation", ylab = "thanatological correlation", asp = 1,
  panel.first = list(
    rect(0,0,1,1,col=gray(.9),border=NA),
    grid(col="white",lty=1),
  segments(0,0,1,1,col="white")))
points(Maler[-TextM1,2],Maler[-TextM1,1], col = "#0000FF50", pch = 19)
text(Maler[TextM1,2],Maler[TextM1,1], varnames[TextM1], cex = 1.2, xpd =TRUE)
dev.off()

pdf("Figures/FemaleCorr1.pdf")
par(mai=c(1,1,.5,.5))
plot(Femaler[,1],Femaler[,2],type="n",xlim=c(0,1),ylim=c(0,1),
  xaxs="i",yaxs="i",cex.axis=1.2,cex.lab=1.2,xlab = "chronological correlation", ylab = "thanatological correlation", asp = 1,
  panel.first = list(
    rect(0,0,1,1,col=gray(.9),border=NA),
    grid(col="white",lty=1),
    segments(0,0,1,1,col="white")))
points(Femaler[-TextF1,2],Femaler[-TextF1,1], col = "#0000FF50", pch = 19)
text(Femaler[TextF1,2],jitter(Femaler[TextF1,1]), varnames[TextF1], cex = 1.2, xpd =TRUE)
dev.off()

pdf("Figures/MaleCorr2.pdf")
par(mai=c(1,1,.5,.5))
plot(Maler[,3],Maler[,4],type="n",xlim=c(0,1),ylim=c(0,1),
  xaxs="i",yaxs="i",cex.axis=1.2,cex.lab=1.2,xlab = "lifespan correlation", ylab = "chrono - thano correlation", asp = 1,
  panel.first = list(
    rect(0,0,1,1,col=gray(.9),border=NA),
    grid(col="white",lty=1),
      segments(0,0,1,1,col="white")))
points(Maler[-TextM2,3],Maler[-TextM2,4], col = "#0000FF50", pch = 19)
text(Maler[TextM2,3],Maler[TextM2,4], varnames[TextM2], cex = 1.2, xpd =TRUE)
dev.off()

pdf("Figures/FemaleCorr2.pdf")
par(mai=c(1,1,.5,.5))
plot(Femaler[,3],Femaler[,4],type="n",xlim=c(0,1),ylim=c(0,1),
  xaxs="i",yaxs="i",cex.axis=1.2,cex.lab=1.2,xlab = "lifespan correlation", ylab = "chrono - thano correlation", asp = 1,
  panel.first = list(
    rect(0,0,1,1,col=gray(.9),border=NA),
    grid(col="white",lty=1),
      segments(0,0,1,1,col="white")))
points(Femaler[-TextF2,3],Femaler[-TextF2,4], col = "#0000FF50", pch = 19)
text(Femaler[TextF2,3],Femaler[TextF2,4], varnames[TextF2], cex = 1.2, xpd =TRUE)
dev.off()


# how many clusters should we make?
#wss <- (nrow(Maler)-1)*sum(apply(Maler,2,var))
#for (i in 2:15) wss[i] <- sum(kmeans(Maler, 
#      centers=i)$withinss)
#plot(1:15, wss, type="b", xlab="Number of Clusters",
#  ylab="Within groups sum of squares")
## the elbow is at 3. so we make 3
#kmeans(Maler,3)


Meta <- read.csv( "Data/PercentThano.csv",stringsAsFactors=FALSE)
Meta <- Meta[, c("Short","Long","Group")]
Meta <- Meta[Meta$Short %in% varnames, ]
Meta <- cbind(Meta,round(Maler[Meta$Short, ],3),round(Femaler[Meta$Short, ],3))

Meta <- Meta[order(Meta$Group), ]
library(xtable)

print(xtable(Meta))
rownames(Meta) <- NULL
MetaL <- split(Meta,Meta$Group)
names(MetaL)
getTablePre <- function(Mi){

   Mi$Group <- NULL
  rws <- seq(1,(nrow(Mi)), by = 2)
  col <- rep("\\rowcolor[gray]{.9}",length(rws))
  print(xtable(Mi),booktabs=TRUE, add.to.row  = list(pos = as.list(rws),command = col),include.rownames=FALSE)
}
getTablePre(MetaL[["ADL"]])
getTablePre(MetaL[["IADL"]])
getTablePre(MetaL[["Behaviors"]])
getTablePre(MetaL[["Functional"]])
getTablePre(MetaL[["Chronic"]])
getTablePre(MetaL[["Cognitive"]])
getTablePre(MetaL[["Psychological"]])
getTablePre(MetaL[["Healthcare"]])
barplot(t(Maler),beside=TRUE)

pdf("Figures/rComparisons/ThanovsChronoMale.pdf")
par(mai = c(1,1,1,1))
plot(Maler[,1], Maler[,2], 
  type = "n",
  xlim=c(0,1), ylim = c(0,1), 
  xlab = "Thano r", ylab = "Chrono r",asp=1, xaxs="i", yaxs="i",
  main = "comparison of correlation coefficients, Males")
abline(a=0,b=1)
text(jitter(Maler[,1]), jitter(Maler[,2]), varnames, cex = .7)
dev.off()

pdf("Figures/rComparisons/ThanovsChronoFemale.pdf")
par(mai = c(1,1,1,1))
plot(Femaler[,1], Femaler[,2], 
  type = "n",
  xlim=c(0,1), ylim = c(0,1), 
  xlab = "Thano r", ylab = "Chrono r",asp=1, xaxs="i", yaxs="i",
  main = "comparison of correlation coefficients, Females")
abline(a=0,b=1)
text(jitter(Maler[,1]), jitter(Maler[,2]), varnames, cex = .7)
dev.off()

pdf("Figures/rComparisons/sexCompareThano.pdf")
par(mai = c(1,1,1,1))
plot(Femaler[,1], Maler[,1], 
  type = "n",
  xlim=c(0,1), ylim = c(0,1), 
  xlab = "Female r", ylab = "Male r",asp=1, xaxs="i", yaxs="i",
  main = "sex comparison of Thano correlation coefficients")
abline(a=0,b=1)
text(jitter(Femaler[,1]), jitter(Maler[,1]), varnames, cex = .7)
dev.off()

pdf("Figures/rComparisons/sexCompareChrono.pdf")
par(mai = c(1,1,1,1))
plot(Femaler[,2], Maler[,2], 
  type = "n",
  xlim=c(0,1), ylim = c(0,1), 
  xlab = "Female r", ylab = "Male r",asp=1, xaxs="i", yaxs="i",
  main = "sex comparison of Chrono correlation coefficients")
abline(a=0,b=1)
text(jitter(Femaler[,2]), jitter(Maler[,2]), varnames, cex = .7)
dev.off()

Meta <- read.csv( "Data/PercentThano.csv",stringsAsFactors=FALSE)
head(Meta)
Meta <- Meta[, c("Short","Long","Group")]

Meta <- Maler[Meta$Short, ]

BB <- cbind(Meta, round(Maler[Meta$Short, ],3), round(Femaler[Meta$Short, ],3))
colnames
BB[order(BB$Group ), ]

#pdf("Figures/SurfExampleFemalesADL3_2.pdf", width = 10, height = 6)
grabber <- paste0("mob","_",.7)
A <- Results[[grabber]][["Male"]]$Surf[,,as.character(1915)]
MaxL <- 2011 - 1915 
A[ col(A) - 1 + 70 + row(A) - 1 > MaxL] <- NA
# possibly need to trim lower left corner too: dimnames(A)
MinL <- 1992 - (1915 + 5)
A[col(A) + 70 - 1 < MinL] <- NA
dev.new(width = 10, height = 6)
SurfMap(A,napprox=9,contour=TRUE,outline=T)
dev.off()

#########################################################
#
# model for Jonas:
# multiplicative give the most freedom.
#mod   <- loess('gross_mot  ~Coh5 * ta * ca' ,
#		data = Dat[Dat$sex == "f", ], 
#		weights = p_wt2, # this, plus point density both act as weights
#		span = span,     # a variable passed in, or smoothness
#		# is similiar conceptually to a 1:1:1 aspect ratio. Everything is in years...
#		normalize = FALSE,
#		control = loess.control(trace.hat="approximate")
#)
#
#save(mod, file = "U:/git/APCT/APCT/Data/gross_mot.Rdata")
#print(object.size(mod), units="Mb")
#Coh5  <- c(1905,1910,1915,1920,1925)
#t.age <- 0:12
#c.age <- 70:100
#
## multiplicative give the most freedom.
#
#newdata        <- expand.grid(ta = t.age+.5, ca = c.age+.5, Coh5 = .Coh5)
## easier to keep dimensions straight if we predict over rectangular grid, 
## then throw out values outside range
## Surf is actually an array
#Surf           <- predict(mod, newdata)
#
#dimnames(Surf) <- list(floor(t.age),floor(c.age), .Coh5)
#
## HRS wave endpoints
#LeftYear <- 1992
#RightYear <- 2011
#
## this reduces extrapolation outside of data points 
#for (i in 1:dim(Surf)[3]){
#	#maxL  <- 2011 - Coh5[i] - 1
#	#maxt  <- tamax[as.character(Coh5[i])]
#	#keept <- as.integer(rownames(Surf)) <= maxt
#	A     <- Surf[,,i]
#	MaxL <- RightYear - .Coh5[i] - 1
#	A[ col(A) - 1 + 70 + row(A) - 1 > MaxL] <- NA
## possibly need to trim lower left corner too: dimnames(A)
#	MinL <- LeftYear - (.Coh5[i] + 5)
#	A[col(A) + 70 - 1 < MinL] <- NA
#	#A[!keept, ] <- NA 
#	Surf[,,i] <- A
#}
#
## and now you can use Surf
#	
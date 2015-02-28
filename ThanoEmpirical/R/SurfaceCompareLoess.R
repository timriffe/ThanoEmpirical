
# Try splitting the data into two birth cohorts and again into two periods 
# and see if there are any essential differences that would hint at/against
# compression

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
#devtools::install_github("timriffe/LexisUtils", subdir = "LexisUtils")

library(LexisUtils)
source("R/SurfMap.R")
library(parallel)


Dat <- local(get(load("Data/Data_long.Rdata")))
SurfaceList <- local(get(load("Data/SurfaceList.Rdata")))


Dat      <- Dat[Dat$age >= 65, ]
Dat      <- Dat[!is.na(Dat$b_yr), ]
Dat$Coh5 <- Dat$b_yr -  Dat$b_yr %% 5 
Coh5keep <- c(1900, 1905, 1910, 1915, 1920, 1925, 1930)
Coh5     <- c(1905, 1910, 1915, 1920, 1925) # i.e. we use the preceding and subsequent cohorts for help fitting
Dat      <- Dat[Dat$Coh5 %in% Coh5keep, ]

#nrow(Dat)
#length(unique(Dat$id))
#nrow(Dat[Dat$Coh5 == 1915,])
#length(unique(Dat$id[Dat$Coh5 == 1915]))
varnames <- names(SurfaceList) 

#Along <- reshape2::melt(A)
#Along2 <- reshape2::melt(t(A))
#coefs <- lm(value~Var1+Var2, data = Along)$coef
#
#summary(lm(value~Var1, data = Along))
#summary(lm(value~Var2, data = Along))
#
#log(coefs[2] / coefs[3])
#
#pca <- princomp(~Var1+Var2, data = Along, center = TRUE, scale = FALSE)
#pca2 <- prcomp(~Var1+Var2, data = Along, center = TRUE, scale = FALSE)
#pca3 <- prcomp(~Var1+Var2, data = Along2, center = TRUE, scale = FALSE)
#cor(duration, waiting) 
#





# first take, block off
runthis <- FALSE
if (runthis){
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
}
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


FitLoess <- function(varname, 
  Dat, 
  sex,
  t.age = 0:12,    # some 5-year cohorts simply don't have 15 years, cut it lower
  c.age = 70:100,  # standard matrix size, though we may NA certain unobserved cells
  span = .5, # will vary
  Coh5){
    # conservative here to cut tails
    maxL  <- 100
    minL  <- 70
    
    # multiplicative give the most freedom.
    mod   <- loess(paste0(varname,'~Coh5 * ta * ca') ,
                    data = Dat[Dat$sex == sex, ], 
                    weights = p_wt2, # this, plus point density both act as weights
                    span = span,     # a variable passed in, or smoothness
                    # is similiar conceptually to a 1:1:1 aspect ratio. Everything is in years...
                    normalize = FALSE 
    )
    
    newdata        <- expand.grid(ta = t.age+.5, ca = c.age+.5, Coh5 = Coh5)
    # easier to keep dimensions straight if we predict over rectangular grid, 
    # then throw out values outside range
    #newdata        <- newdata + .5
    Surf           <- predict(mod, newdata)
 
    dimnames(Surf) <- list(floor(t.age),floor(c.age), Coh5)

    # this reduces extrapolation outside of data points 
    for (i in 1:dim(Surf)[3]){
      #maxL  <- 2011 - Coh5[i] - 1
      #maxt  <- tamax[as.character(Coh5[i])]
      #keept <- as.integer(rownames(Surf)) <= maxt
      A     <- Surf[,,i]
      MaxL <- 2011 - Coh5[i] 
      A[ col(A) - 1 + 70 + row(A) - 1 > MaxL] <- NA
# possibly need to trim lower left corner too: dimnames(A)
      MinL <- 1992 - (Coh5[i] + 5)
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
if(do.this){
Results <- mclapply(allcomboswide, function(x,Dat,Coh5){
    cat(x[1],"Female\n")
    Female <- try(FitLoess(varname = x[1], 
      Dat = Dat, 
      sex = "f",
      t.age = 0:12,    # some 5-year cohorts simply don't have 15 years, cut it lower
      c.age = 70:100,  # standard matrix size, though we may NA certain unobserved cells
      span =  as.numeric(x[2]), # will vary
      Coh5 = Coh5))
    cat(x[1],"Male\n")
    Male <- try(FitLoess(varname = x[1], 
      Dat = Dat, 
      sex = "m",
      t.age = 0:12,    # some 5-year cohorts simply don't have 15 years, cut it lower
      c.age = 70:100,  # standard matrix size, though we may NA certain unobserved cells
      span =  as.numeric(x[2]), # will vary
      Coh5 = Coh5))
    list(Male = Male, Female = Female)
  }, Dat = Dat, Coh5 = Coh5, mc.cores = detectCores())

save(Results,file="Data/LoessQuinquenal.Rdata")
}

Results <- local(get(load("Data/LoessQuinquenal.Rdata")))

# TODO: first thing: produce surfaces: many of them!
# probably the 1910,15,20 surfaces stacked in rows,
# with span in columns. Best way to summarize.
# this way we choose a span, a cohort, and which two variables.

names(Results) <- unlist(lapply(Results, function(X){
    paste0(X$Male$varname,"_", X$Male$span)
  }))

#which(unlist(lapply(Results,function(X){
#    class(X$Female) == "try-error"
#  })))



cellwidths <- c(1,3,3,3,1)
cellheights <- c(1,2,2,2,2,2)
plotn <- function(xlim = c(0,1),ylim = c(0,1), mai = c(0,0,0,0)){
  plot(NULL, type = "n", xlim = xlim, ylim = ylim,  axes = FALSE, xlab = "", ylab = "")
}

#.varname <- "srh"
#.sex <- "Male"
#.span <- .5
#.coh <- 1915
#.Results <- Results

SurfA <- function(.varname,.sex,.span,.coh,.Results,.ticks){
  grabber <- paste0(.varname,"_",.span)
  A <- .Results[[grabber]][[.sex]]$Surf[,,as.character(.coh)]
  
  MaxL <- 2011 - .coh - 1
  A[! col(A) - 1 + 70 + row(A) - 1 < MaxL] <- NA
  # possibly need to trim lower left corner too: dimnames(A)
  MinL <- 1992 - (.coh + 5)
  A[col(A) + 70 - 1 <= MinL] <- NA
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
    mai = c(.1,.1,.1,.1))
}

ticks <- pretty(Results[[paste0("back","_",.5)]][["Female"]]$Surf,n=10)
SurfA("back","Female",.5,Coh5[1],Results,ticks)
1992-1919-1

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

graphics.off()
dev.new(width = sum(cellwidths), height = sum(cellheights))


pdf("Figures/PanelCoh5/Females.pdf",width = sum(cellwidths), height = sum(cellheights))
lapply(varnames, function(x){
    makePanel(x,"Female")
  })
dev.off()



pdf("Figures/PanelCoh5/Males.pdf",width = sum(cellwidths), height = sum(cellheights))
lapply(varnames, function(x){
    makePanel(x,"Male")
  })
dev.off()

get_r <- function(A){
  Along <- reshape2::melt(A)
  c(Thano = abs(cor(Along$value,Along$Var1,use="complete.obs")), 
  Chrono = abs(cor(Along$value,Along$Var2,use="complete.obs")))
}
library(LexisUtils)


Maler <- do.call(rbind,lapply(varnames, function(x, Results){
    grabber <- paste0(x,"_",.7)
    A <- Results[[grabber]][["Male"]]$Surf[,,"1915"]
    get_r(A)
  }, Results = Results))
rownames(Maler) <- varnames

Femaler <- do.call(rbind,lapply(varnames, function(x, Results){
      grabber <- paste0(x,"_",.7)
      A <- Results[[grabber]][["Female"]]$Surf[,,"1915"]
      get_r(A)
    }, Results = Results))
rownames(Femaler) <- varnames


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
SurfMap(A,napprox=9,contour=TRUE,outline=FALSE)
dev.off()





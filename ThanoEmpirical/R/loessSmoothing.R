# TR
# This script follows CreateMatrices.R 
# this is where the smoothing happens. We smooth over thanatological age, 
# chronological age, and 5-year birth cohorts.
# Using a loess smoother is rough around the edges, literally, as it may have
# edge-effects, and we definitely don't do anything about within-person
# autocorrelation. Not worries about edge-effects (at thanatological age 0)
# because raw means shows them, and we think it's real. In any case,
# the correlations analysis is really rough, and wouldn't pick up a super-steep
# ascent anyway. That's why we're not worried about getting the estimate perfect.
# My guess is that the estimates for many variables in thanatological age 0 are
# underestimates.
# This script is followed by Correlations.R
###############################################################################


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


Dat           <- local(get(load("Data/Data_long.Rdata")))
SurfaceList   <- local(get(load("Data/SurfaceList.Rdata")))
varnames      <- names(SurfaceList)


# cut data down, define time vars

# no ages under 65
Dat      <- Dat[Dat$age >= 65, ]

# birth year must be known
Dat      <- Dat[!is.na(Dat$b_yr), ]

# group to quinquennial
Dat$Coh5 <- Dat$b_yr -  Dat$b_yr %% 5 

# Coh5keep are cohorts kept for fitting
Coh5keep <- c(1900, 1905, 1910, 1915, 1920, 1925, 1930)

# Cog5 are cohorts for predicting
Coh5     <- c(1905, 1910, 1915, 1920, 1925) # i.e. we use the preceding and subsequent cohorts for help fitting

# select only cohorts used for fitting
Dat      <- Dat[Dat$Coh5 %in% Coh5keep, ]

# a final check
all(varnames %in% colnames(Dat))

#------------------------------------------
# define loess function, does some post-processing too:
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
	Dati  <- Dat[Dat$sex == sex, ]
	
	# multiplicative gives the most freedom. ta and ca are decimals
	mod   <- loess(paste0(varname,'~Coh5 * ta * ca') ,
					data = Dati, 
					weights = p_wt2, # this, plus point density both act as weights
					span = span,     # a variable passed in, or smoothness
									 # is similiar conceptually to a 1:1:1 aspect ratio. 
			                         # Everything is in years...
					normalize = FALSE,
					control = loess.control(trace.hat = "approximate")
	)
	# use midpoints for chrono and thano age
	newdata        <- expand.grid(ta = t.age + .5, ca = c.age + .5, Coh5 = .Coh5)
	
	# easier to keep dimensions straight if we predict over rectangular grid, 
	# then throw out values outside range
	#newdata        <- newdata + .5
	Surf           <- predict(mod, newdata)
	
	dimnames(Surf) <- list(floor(t.age), floor(c.age), .Coh5)
	
	# need to trim on the left side where applicable, since some questions didn't enter until
	# wave 2 or 3. There are many such cases, so check and make sure we dont' extrapolate.
	# sex <- "m"
	# varname <- "cesd"
	MissingWaves <- tapply(Dati[, varname], Dati[, "wave"], function(x){
				all(is.na(x))
			})
	LeftYear  <- 1992
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
		A     <- Surf[, , i]
		MaxL  <- RightYear - .Coh5[i] - 1
		A[ col(A) - 1 + 70 + row(A) - 1 > MaxL] <- NA
        # possibly need to trim lower left corner too: dimnames(A)
		MinL  <- LeftYear - (.Coh5[i] + 5)
		A[col(A) + 70 - 1 < MinL] <- NA
		#A[!keept, ] <- NA 
		Surf[, , i] <- A
	}
	list(Surf = Surf, span = span, sex = sex, varname = varname, Cohorts = Coh5)
}

#------------------------------------------

# object used for parameters iterated over
allcombos     <- expand.grid(varname = varnames, span = c(.5,.7,.9))
# divide into list for lapply
allcomboswide <- as.data.frame(t(allcombos),stringsAsFactors = FALSE)


# since this is a big job, by default we don't do it unless manually toggled here:
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
	
	# give names to list so we can find stuff 
	names(Results) <- unlist(lapply(Results, function(X){
						paste0(X$Male$varname,"_", X$Male$span)
					}))
	
	# and save out if done:
	save(Results,file="Data/LoessQuinquenal.Rdata")
}




# Author: tim
###############################################################################
# Instructions: 
# tend to steps 1-3 as appropriate, explained in situ.
# the script will produce two figures (optional) and
# two Rdata objects called CorrelationResultsBoot.Rdata 
# and ResultsLongBoot.Rdata will be created, which Tim will
# want :-)
# ------------------


# this script mirrors the script ns_glm_smoothing.R, but differs in one major respect:
# We do a large bootstrap, where sampling with replacement is done on *first* 
# observations, then selecting all posterior observations of each individual. We repeat
# 1000 (or more) times and then take the mean

# ------------------------
# 1) set parameters
nboot        <- 10000    # ? how many should we do?
do.this      <- FALSE    # change this to TRUE
make.figs    <- TRUE     # shall we make the summary historgrams?

# 2) set working directory: you'll need to modify the working dir. Possibly by generalizing the below
# code or else commenting it out altogether and setting manually. Up to you
# ------------------------
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

# 3) last thing to check:
# load in data, either change this path (commenting this one out so I keep it)
# or else make sure Data_long.Rdata is in a folder called Data inside the working directory...
Dat        <- local(get(load("Data/Data_long.Rdata")))

# the rest should work fine without further ado. Two figures will also be created 
# in the working directory. These can be examined or thrown out.

# ------------------------
# get packages loaded
# ------------------------
# gives mclapply()-like functionality in Windows. 
# I usually would use parallel package, but that won't cut it on Windows
if (.Platform$OS.type == "windows"){
	# does Hydra have devools? I hope so.
	if (!"parallelsugar" %in% rownames(installed.packages())){
		library(devtools)
		install_github('nathanvan/parallelsugar')
	}
	library(parallelsugar)
} else {
	# this is for all else (Tim)
	library(parallel)
}

library(splines)
library(data.table)
library(reshape2)
library(lattice)
# ------------------------
# the following functions are defined here to 
# avoid having to source and set another path.

# ------------------------
# a utility
cutla <- function(newdata, year1 = 1992, year2 = 2011, maxl = 100){
	# cut age
	newdata$la <- newdata$ca + newdata$ta
	mini       <- newdata$ca >= (year1 - newdata$b_yr - 1)
	maxi       <- newdata$la < (year2 - newdata$b_yr)
	newdata    <- newdata[mini & maxi, ]
	newdata    <- newdata[newdata$la < maxl, ]
	newdata
}

# This is a general boot function for this particular dataset and method,
# takes care of some data management too. Edge oversampling is by default turned
# off, but can be controlled with YearsFromEdge and MagFactor

# ------------------------
apct.boot <- function(
		data,
		varname = "adl3_",
		nboot = 250,
		t.age = 0:12,     
		c.age = 70:100,
		b_yr_range = min(data$b_yr):max(data$b_yr),
		YearsFromEdge = 0,          # for optional edge diagnostics, which we'll ignore here.
		MagFactor = 1) {
	# data is the HRS object, selected for dead people
	# and whatever other data prep was required, e.g. to
	# make vars binary if wished
	# varname is 
	
	# this line super important 
	data            <- data[order(data$id), ]
	
	# determine potential edge cases
	data$edgies   <- data$la_int > 2011 - data$b_yr - YearsFromEdge
	if (YearsFromEdge == 0){
		data$edgies <- FALSE
	}
	
	# select first cases, for sampling
	ids          <- data$id
	lengths      <- rle(ids)$lengths
	select.first <- cumsum(lengths) - lengths + 1
	dataid       <- data[select.first, ]
	
	# another object useful for resampling
	rows         <- 1:nrow(data)
	rowlist      <- split(rows, ids)
	
	dataid$drawweight                	<- dataid$p_wt2
	
	# this either will or won't have an effect depending on edgies and mag factor
	# by default no edge oversampling
	dataid$drawweight[dataid$edgies] 	<- dataid$drawweight[dataid$edgies] * MagFactor
	
	# assign person weight of first observation to each subsequent observation
	data$firstweight                    <- rep(dataid$drawweight, lengths)
	# given repalcement selection by a draw weight, the remaining observations
	# of individuals are thusly reweighted
	data$rescaleweight                  <- data$p_wt2 / data$firstweight
	
	# reduce size of object to just required columns
	col.index.outc 	<- grep(paste0('^', varname, '$'), colnames(data))
	col.index.id 	<- grep(paste0('^', 'id', '$'), colnames(data))
	col.index.b_yr 	<- grep(paste0('^', 'b_yr', '$'), colnames(data))
	col.index.ta 	<- grep(paste0('^', 'ta', '$'), colnames(data))
	col.index.ca 	<- grep(paste0('^', 'ca', '$'), colnames(data))
	col.index.rscw 	<- grep(paste0('^', 'rescaleweight', '$'), colnames(data))
	colselect       <- c(col.index.outc, col.index.id, col.index.rscw, col.index.b_yr, 
			col.index.ta, col.index.ca, col.index.rscw)
	data            <- data[, colselect]
	gc()
	
	# and for those that are also edgies we need to weight in the opposite direction
	# same magnitude (but there will be more such people)
	# this line redundant, since we now divide by drawweight (by default equal to p_wt2)
	# Dat$rescaleweight[Dat$edgies]       <- Dat$rescaleweight[Dat$edgies] / MagnificationFactor
	
	# cut down dataid object to two needed columns, id and drawweight
	dataid     <- dataid[,c("id","drawweight")]
	
	
	# data for prediction, grid
	newdata    <- expand.grid(ta = t.age+.5, 
			ca = c.age+.5, 
			b_yr = b_yr_range)
	
	# remove extrapolation points for glm prediction
	out        <- cutla(newdata, year1 = 1992, year2 = 2011)
	
	# number of cells in the jacked up Lexis surface that we actually need estimates for
	ncell      <- nrow(out)
	
	# matrix in which we save our estimates
	bootsave   <- matrix(ncol = ncell, nrow = nboot)
	idlength   <- nrow(dataid)
	
	for(b in 1:nboot) {
		# draw IDs with replacement and with weight
		selectid    <- sample(dataid[, 1],
				size = idlength,
				replace = TRUE,
				prob = dataid[,2])
		
		select.rows <- unlist(rowlist[as.character(selectid)])
		data.boot   <- data[select.rows, ]
		
		## perform the ns code
		col.index   <- grep(paste0('^', varname,'$'), colnames(data.boot))
		
		if(max(data.boot[, col.index], na.rm = TRUE) == 1) {
			# determine if data is binary or not
			# if binary do (quasi)binomial glm
			fit        <- glm(data.boot[, col.index] ~ 
							ns(b_yr, knots = seq(1902.5, 1925.5, by = 5)) + 
							ns(ta, knots = c(.5, 1, 2, 4, 7.5, 10)) +  
							ns(ca, knots = seq(72.5, 97.5, by = 5)), 
					data = data.boot,
					weights = rescaleweight,
					family = quasibinomial)
			
		} else { # if not binary do lr glm
			fit        <- glm(data.boot[, col.index] ~ 
							ns(b_yr, knots = seq(1902.5, 1925.5, by = 5)) + 
							ns(ta, knots = c(.5, 1, 2, 4, 7.5, 10)) +  
							ns(ca, knots = seq(72.5, 97.5, by = 5)), 
					data = data.boot,
					weights = rescaleweight)
		}
		
		# easier to keep dimensions straight if we predict over rectangular grid, 
		# then throw out values outside range
		# output so that bootstrap function can ...bootstrap it
		bootsave[b, ] <- predict(fit, out, type = 'response')
		
		print(b)
		
	}
	# don't need last estimate
	out$pi      <- NULL
	# change centroids to lower bounds for easier plotting
	out$ta     	<- floor(out$ta)
	out$ca     	<- floor(out$ca)
	
	# return both vec and it's named dimensions
	return(list(boot.est = bootsave,
					dims = out))
	
}
# utility, called in get.booty()
boot.ci <- function(bootdata,conf.level) {
	# bootdata is a dataframe or matrix like bootsave
	# conf.level is the confidence level we want
	# e.g. a 95% CI, a 99% CI, etc.
	
	# CI bounds
	lower <- (1-conf.level)/2
	upper <- conf.level+lower
	
	return(apply(bootdata,2,quantile,probs=c(lower,upper)))
	
}
# mean, median, 2.5% and 97.5% quantiles. Really all we need
# in the present script is either the mean or median values.
get.booty <- function(boot.list){
	# separate parts
	dims        <- boot.list$dims
	boot.est    <- boot.list$boot.est
	dims$mean   <- apply(boot.est, 2, median, na.rm = TRUE)
	dims$median <- apply(boot.est, 2, mean, na.rm = TRUE)
	dims        <- cbind(dims, t(boot.ci(boot.est, .95)))
	dims$Width  <- c(diff(t(dims[,c("2.5%","97.5%")])))
	dims
}
# turn any of the above columns into a TTD by age matrix for a selected cohort
# probably won't need this, but could be useful
get.mat <- function(goods, column = "median", cohort = 1915){
	acast(goods[goods$b_yr == cohort,], ta ~ ca, value.var = column)
}


# no ages under 65
#oops age is in months!
Dat        <- Dat[(Dat$age / 12) >= 65, ]
# birth year must be known
Dat        <- Dat[!is.na(Dat$b_yr), ]
# group to quinquennial
Dat$Coh5   <- Dat$b_yr - Dat$b_yr %% 5 
# Coh5keep are cohorts kept for fitting
Coh5keep   <- seq(1900, 1930, by = 5) 
# select only cohorts used for fitting
Dat        <- Dat[Dat$Coh5 %in% Coh5keep, ]
## integer age representations
Dat$ta_int <- floor(Dat$ta)
Dat$ca_int <- floor(Dat$ca)
Dat$la_int <- floor(Dat$ta + Dat$ca)
# let's remove people with ta = -1
Dat        <- Dat[Dat$ta >= 0,]
# even tho most neg are very close to zero

# TR: I think this list is already solidified, but just in case.
varnames <- c("adl3_", 
		"adl5_", "iadl3_", "iadl5_", "cesd",  "lim_work", "srh", 
		"bmi", "back", "hosp", "hosp_stays", "hosp_nights", "nh", 
		"nh_stays", "nh_nights", "nh_now", "nh_mo", "nh_yr", "nh_days", 
		"doc", "doc_visits", "hhc", "meds", "surg", "dent", "shf", "adl_walk", 
		"adl_dress", "adl_bath", "adl_eat", "adl_bed", "adl_toilet", 
		"iadl_map", "iadl_tel", "iadl_money", "iadl_meds", "iadl_shop", 
		"iadl_meals", "mob", "lg_mus", "gross_mot", "fine_mot", "bp", 
		"diab", "cancer", "lung", "heart", "stroke", "psych", "arth", 
		"cc", "alc_ev", "alc_days", "alc_drinks", "smoke_ev", "smoke_cur", 
		"cesd_depr", "cesd_eff", "cesd_sleep", "cesd_happy", "cesd_lone", 
		"cesd_sad", "cesd_going", "cesd_enjoy", "prob75yo", "alz", "dem", 
		"srm", "pastmem", "ss", "c20b", "name_mo", 
		"name_dmo", "name_yr", "name_dwk", "name_sci", "name_cac", "name_pres", 
		"name_vp", "vocab", "tm", "med_exp", "dwr","twr","iwr",
		"iadl_calc", "mprob", "mprobev", "med_explog") 


# pare down to columns available in current iteration
varnames <- varnames[varnames %in% colnames(Dat)]

# set appropriate number of cores to do the work.
ncores <- ifelse(detectCores() > 8, 8, (detectCores() - 1))

# TR: this is toggled at the head of the script.
# just to make sure resources not too tied up
if (do.this){
	boot.results.list <- mclapply(varnames, function(varname, Dat, nboot){
				fem 	<- get.booty(apct.boot(Dat[Dat$sex == "f", ], varname = varname, nboot = nboot,
								YearsFromEdge = 0, MagFactor = 1))
				mal 	<- get.booty(apct.boot(Dat[Dat$sex == "m", ], varname = varname, nboot = nboot,
								YearsFromEdge = 0, MagFactor = 1))
				fem$Sex <- "f"
                mal$Sex <- "m"
                out 	<- rbind(fem,mal)
				out$var <- varname
				out
			}, Dat = Dat,  
			   nboot = nboot,    # nboot is set at the head of the script.
			   mc.cores = ncores)# ncores is set just above here
}

ResultsLong         <- do.call(rbind, boot.results.list)

ResultsLong$Cohort  <- ResultsLong$b_yr - ResultsLong$b_yr %% 5

# take simple means within ca, ta, Cohort, var, Sex
ResultsLong   		<- data.table(ResultsLong)

# choosing median here, could just as well be mean.
ResultsLong5  		<- ResultsLong[, list(pi = mean(median, na.rm=TRUE)), by = list(var, Sex, Cohort, ca, ta )]
ResultsLong5  		<- as.data.frame(ResultsLong5)
ResultsLong5L 		<- split(ResultsLong5,with(ResultsLong5, list(var, Sex, Cohort)))

# do a quick comparison with ThanoEmpirical results:

# function used, given a thano x chrono surface:
get_r <- function(A){
	c(T  = abs(cor(A$pi, A$ta, use = "complete.obs")), 
			A  = abs(cor(A$pi, A$ca, use = "complete.obs")),
			L  = abs(cor(A$pi, A$ta + A$ca, use = "complete.obs")),
			M  = abs(cor(A$pi, A$ca - A$ta, use = "complete.obs"))
	)
}

# --------------------------------------------
# This gets the correlations for each dim, cohort, sex, and span.
Results_r <- do.call(rbind,
		lapply(ResultsLong5L, function(X){
					out     <- X[1:4, ]
					out$Dim <- c("T","A","L","M")
					out$r   <- get_r(X)
					out$pi  <- NULL
					out$ca  <- NULL
					out$ta  <- NULL
					out
				})
)

comparison.tim <- FALSE
if (comparison.tim){
# compare these with the original results
Results_loess_r   	<- local(get(load("Data/Correlations.Rdata")))

# match ordering
Results_r       	<- Results_r[with(Results_r,order(var, Sex, Cohort, Dim)), ]
Results_loess_r   	<- Results_loess_r[with(Results_loess_r,order(var, sex, Cohort, Dim)), ]
Results_r$var   	<- gsub("_", "",Results_r$var)

Results_loess_r.5 	<- Results_loess_r[Results_loess_r$span == "0.5", ]
Results_loess_r.7 	<- Results_loess_r[Results_loess_r$span == "0.7", ]
Results_loess_r.9 	<- Results_loess_r[Results_loess_r$span == "0.9", ]
}
# match cohorts
Results_r       	<- Results_r[Results_r$Cohort %in% unique(Results_loess_r.5$Cohort), ]

# TR: hmmm. positive correlation, but that cloud is fatter than
# I'd like!
#plot(Results_r$r, Results_loess_r.5$r)
#plot(Results_r$r[Results_r$Cohort == 1915], Results_loess_r.5$r[Results_loess_r.5$Cohort == 1915])

# so let's start be redoing Figure 5a, 5b too see if they change much

Hist7      <- Results_r[Results_r$Cohort == 1915, ]
Hist7$rbin <- round(Hist7$r * 100) %/% 10 * 10
Hist7$rbin <- as.factor(Hist7$rbin)
Hist7$R    <- Hist7$r * 100
Hist7$Dim  <- as.factor(Hist7$Dim )

if (make.figs){
# Figures that ought to be mostly comparable with
# Figure 5 in ThanoEmpirical paper. Hopefully
# these histograms have a similar shape!!

pdf("Figure5a_v3.pdf", width = 3, height = 7)
histogram(~R | Dim, 
		data = Hist7[Hist7$Sex == "f", ], 
		col = gray(.4), 
		par.settings = list(strip.background = list(col = gray(.9))), 
		layout = c(1,4), 
		type = "count",
		breaks = seq(0,100, by = 10),
		index.cond = list(c(3, 4, 1, 2)), 
		xlab = list(label = "correlation coef * 100"),
		ylab = list(label = "variable count"),
		ylim = c(0, 47))
dev.off()

pdf("Figure5b_v3.pdf", width = 3, height = 7)
histogram(~R | Dim, 
		data = Hist7[Hist7$Sex == "m", ], 
		col = gray(.4), 
		par.settings = list(strip.background = list(col = gray(.9))), 
		layout = c(1, 4), 
		type = "count",
		breaks = seq(0, 100, by = 10),
		index.cond = list(c(3, 4, 1, 2)), 
		xlab = list(label = "correlation coef * 100"),
		ylab = list(label = "variable count"),
		ylim = c(0, 47))
dev.off()

} # end fig chunk


# TR will want a copy of this output.
save(ResultsLong, file = "ResultsLongBoot.Rdata")
save(Results_r, file = "CorrelationResultsBoot.Rdata")

getwd()


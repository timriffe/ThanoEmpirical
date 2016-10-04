# TR
# this script follows loessSmoothing.R
# it does the easy-peasy correlations analysis (though the code may look exotic)
# This script is followed by PaperFigures.R and AppendixHeatTables.R
###############################################################################

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
# --------------------------------
# load in loess results
Results <- local(get(load("Data/LoessQuinquenal.Rdata")))

#-------------------------------
# function used, given a thano x chrono surface:
get_r <- function(A){
	# it's pretty simple, really.
	# we take the absolute value because it shouldn't matter 
	# whether high is bad and low is good: i.e. numeric coding. 
	Along <- reshape2::melt(A)
	c(T  = abs(cor(Along$value, Along$Var1, use = "complete.obs")), 
			A  = abs(cor(Along$value, Along$Var2, use = "complete.obs")),
			L  = abs(cor(Along$value, Along$Var2 + Along$Var1, use = "complete.obs")),
			M  = abs(cor(Along$value, Along$Var1 - Along$Var2, use = "complete.obs"))
	)
}
# --------------------------------

# let's first purge to remove negatives.
Results <- lapply(Results, function(x){
			x$Female$Surf[x$Female$Surf < 0] <- NA
			x$Male$Surf[x$Male$Surf < 0]    <- NA
			x
		})

# --------------------------------------------
# This gets the correlations for each dim, cohort, sex, and span.
Results_r <- do.call(rbind,
		lapply(Results, function(x){
					Surfsm     <- x$Male$Surf
					outm       <- reshape2::melt(
							apply(Surfsm, 3, get_r),
							varnames = c("Dim","Cohort"),
							value.name = "r")
					outm$var   <- x$Male$varname
					outm$span  <- x$Male$span
					outm$sex   <- "m"
					Surfsf     <- x$Female$Surf
					outf       <- reshape2::melt(
							apply(Surfsf ,3, get_r),
							varnames = c("Dim","Cohort"),
							value.name = "r")
					outf$var   <- x$Female$varname
					outf$span  <- x$Female$span
					outf$sex   <- "f"
					out <- rbind(outf,outm)
					out$Dim <- as.character(out$Dim)
					out
				})
)
# --------------------------------------------

# dunno why got in habit of using the surflist as a means
# of keeping names. I guess that's where I did the best job
# of restricting vars to those with OK patterns, because those
# surfaces are just empirical means without smoothing, so breaks
# are more evident, ergo those varnames were the most up-to-date. hmmm
SurfaceList    <- local(get(load("Data/SurfaceList.Rdata")))
varnames       <- names(SurfaceList)

# remove underscore, LaTeX has probs with it
varnames       <- gsub("_","", varnames)
Results_r$var  <- gsub("_","", Results_r$var)



# remove extra vars, if any, using SurfaceList object names
Results_r             <- Results_r[Results_r$var %in% varnames, ]

# save as intermediate data step.
save(Results_r, file = "Data/Correlations.Rdata")

# end

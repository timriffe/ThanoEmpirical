# Tr:
# This script gets the correlation directions for each matrix
# The script follows Correlations.R
# Author: tim
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
# load in correlation results, generated in Correlations.R
Results_r <- local(get(load("Data/Correlations.Rdata")))


# select span == "0.7"
Res.7 <- Results_r[Results_r$span == "0.7", ]

# --------------------------------------------
# functions to create heattable rows
row.heat <- function(x){
	x   <- x[c("L", "A", "T", "M")]
	n   <- length(x)
	nm1 <- n - 1
	par(xaxs = "i", yaxs = "i", mai = c(.03, .03, .03, .03))
	plot(NULL, 
			xlim = c(0, n), 
			ylim = c(0, 1), 
			axes = FALSE, 
			xlab = "", 
			ylab = "", 
			asp = 1)
	rect(0:nm1, 0, 1:n, 1, 
			border = "white", 
			lwd = 3, 
			col = gray(1 - x))
	text(0:nm1 + .5, .5, round(x * 100), 
			col = ifelse(x < .5, "black", "white"), 
			cex = 2)
}
# selection functiln, should return named vector of length 4
grabr <- function(Dat, sex, cohort, varname){
	ind        <- Dat$Cohort == cohort & Dat$sex == sex & Dat$var == varname
	out        <- Dat$r[ind]
	names(out) <- Dat$Dim[ind]
	out
}
# --------------------------------------------
# create heatmap cells for all cohorts

# cohort <- coh, varname = "adl3"
for (coh in seq(1905, 1925, by = 5)){
	for (sex in c("m", "f")){
		for (v in varnames){
			path <- file.path("Figures/HeatTables",
					coh, paste0(sex, v, ".pdf"))
			#x <- grabr(Res.7, sex, coh, v)
			pdf(path, height = 1.06, width = 4.06)
			row.heat(grabr(Dat = Res.7, 
						   sex = sex, 
						   cohort = coh,
						   varname = v))
			dev.off()
		}
	}
}
# the rest of the appendix tables is done in LaTeX
# --------------------------------------------

# -----------------------------------------
# Save out all correlation results to csv
Results_Out              <- Results_r
# caps, to match appendix tables
Results_Out$var          <- toupper(Results_Out$var)
# remove row names
rownames(Results_Out)    <- NULL
# change Dim to Pattern
colnames(Results_Out)[1] <- "Pattern"
# Save
write.csv(Results_Out, 
		file = "Appendix/Results.csv",
		row.names = FALSE)
dim(Results_Out) # manageable size anyway
# -----------------------------------------

# end
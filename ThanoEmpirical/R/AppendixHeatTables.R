
# Author: tim
###############################################################################

Results <- local(get(load("Data/LoessQuinquenal.Rdata")))

get_r <- function(A){
	# it's pretty simple, really.
	# we take the absolute value because it shouldn't matter 
	# whether high is bad and low is good: i.e. numeric coding. 
	Along <- reshape2::melt(A)
	c(T  = abs(cor(Along$value, Along$Var1, use = "complete.obs")), 
			A   = abs(cor(Along$value, Along$Var2, use = "complete.obs")),
			L = abs(cor(Along$value, Along$Var2 + Along$Var1, use = "complete.obs")),
			M    = abs(cor(Along$value, Along$Var1 - Along$Var2, use = "complete.obs"))
	)
}

##########################
# OK, let's instead get r for every surface, all spans.

# let's first purge to remove negatives.
Results <- lapply(Results, function(x){
			x$Female$Surf[x$Female$Surf < 0] <- NA
			x$Male$Surf[x$Male$Surf < 0] <- NA
			x
		})

Results_r <- do.call(rbind,lapply(Results, function(x){
					Surfsm     <- x$Male$Surf
					outm       <- reshape2::melt(apply(Surfsm,3,get_r),varnames=c("Dim","Cohort"),value.name="r")
					outm$var   <- x$Male$varname
					outm$span  <- x$Male$span
					outm$sex   <- "m"
					Surfsf     <- x$Female$Surf
					outf       <- reshape2::melt(apply(Surfsm,3,get_r),varnames=c("Dim","Cohort"),value.name="r")
					outf$var   <- x$Female$varname
					outf$span  <- x$Female$span
					outf$sex   <- "f"
					rbind(outf,outm)
				}))
head(Results_r)

#graphics.off()
#hist(with(Results_r,r[Dim == "M" & span == "0.5" ]))
#dev.new()
#hist(with(Results_r,r[Dim == "M" & span == "0.9" ]))

# remove extra var
Results_r <- Results_r[Results_r$var %in% varnames, ]

# select span == "0.7"
Res.7 <- Results_r[Results_r$span == "0.7", ]

# now make a function that plots a row for a variable and sex:

# 1) heat boxes
# 2) partial filled boxes.




row.heat <- function(x){
	x   <- x[c("L","A","T","M")]
	n   <- length(x)
	nm1 <- n - 1
	par(xaxs="i",yaxs="i",mai=c(.03,.03,.03,.03))
	plot(NULL, xlim=c(0,n),ylim=c(0,1),axes=FALSE, xlab="",ylab="",asp=1)
	rect(0:nm1,0,1:n,1,border="white",lwd=3,col=gray(1-x))
}
#row.boxes <- function(x){
#	x   <- x[c("L","A","T","M")]
#	n   <- length(x)
#	nm1 <- n - 1
#	par(xaxs="i",yaxs="i",mai=c(.03,.03,.03,.03))
#	plot(NULL, xlim=c(0,n),ylim=c(0,1),axes=FALSE, xlab="",ylab="",asp=1)
#	rect(0:nm1,0,1:n,1,border=gray(.7),lwd=3)
#	rect(0:nm1,0,0:nm1+sqrt(x),sqrt(x),border=NA,col="black")
#}
#x <- grabr(Res.7,"m",1920,"adl3_")
grabr <- function(Dat,sex,cohort,varname){
	ind <- with(Dat,Cohort==1925 & sex == "m" & var == "adl3_")
	out <- Dat$r[ind]
	names(out) <-  Dat$Dim[ind]
	out
}

#
#row.boxes(grabr(Res.7,"m",1920,"adl3_"))
row.heat(grabr(Res.7,"m",1920,"adl3_"))

for (coh in seq(1905,1925,by=5)){
	for (sex in c("m","f")){
		for (v in varnames){
			path <- file.path("/home/tim/git/ThanoEmpirical/ThanoEmpirical/Figures/HeatTables",
					coh,paste0(sex, v, ".pdf"))
			pdf(path, height = 1.06, width = 4.06)
			row.heat(grabr(Res.7, sex, coh, v))
			dev.off()
		}
	}
}


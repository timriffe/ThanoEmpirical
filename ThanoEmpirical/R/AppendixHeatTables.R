# This script gets the correlation directions for each matrix
#
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
			x$Male$Surf[x$Male$Surf < 0] <- NA
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
head(Results_r[Results_r$sex == "f", ])
head(Results_r[Results_r$sex == "m", ])
# --------------------------------------------


#graphics.off()
#par(mfrow=c(1,2))
#hist(with(Results_r,r[Dim == "M" & span == "0.5" ]))
#hist(with(Results_r,r[Dim == "M" & span == "0.9" ]))

SurfaceList   <- local(get(load("Data/SurfaceList.Rdata")))
varnames      <- names(SurfaceList)

# remove underscore, LaTeX has probs with it
varnames <- gsub("_","", varnames)
Results_r$var  <- gsub("_","", Results_r$var)



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
	par(xaxs = "i", yaxs = "i", mai = c(.03, .03, .03, .03))
	plot(NULL, xlim = c(0, n), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "", asp = 1)
	rect(0:nm1, 0, 1:n, 1, border = "white", lwd = 3, col = gray(1 - x))
	text(0:nm1 + .5, .5, round(x * 100), col = ifelse(x < .5, "black", "white"), cex = 2)
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

grabr <- function(Dat, sex, cohort, varname){
	ind        <- Dat$Cohort == cohort & Dat$sex == sex & Dat$var == varname
	out        <- Dat$r[ind]
	names(out) <- Dat$Dim[ind]
	out
}

#
x <- grabr(Res.7,"m",1915,"adl3")
row.heat(grabr(Res.7,"m",1915,"adl3"))
row.heat(grabr(Res.7,"f",1915,"adl3"))
#coh <- 1915;sex <- "f"
# cohort <- coh, varname = "adl3"
for (coh in seq(1905,1925, by = 5)){
	for (sex in c("m","f")){
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
sort(varnames)

# ---------------------------------------------
# make four histograms:
Hist7 <- Res.7[Res.7$Cohort == 1915, ]
#
hist(Hist7[Hist7$sex == "f" & Hist7$Dim == "L", "r"])
hist(Hist7[Hist7$sex == "f" & Hist7$Dim == "A", "r"])
hist(Hist7[Hist7$sex == "f" & Hist7$Dim == "T", "r"])
hist(Hist7[Hist7$sex == "f" & Hist7$Dim == "M", "r"])
#

hist(Hist7[Hist7$sex == "m" & Hist7$Dim == "L", "r"])
hist(Hist7[Hist7$sex == "m" & Hist7$Dim == "A", "r"])
hist(Hist7[Hist7$sex == "m" & Hist7$Dim == "T", "r"])
hist(Hist7[Hist7$sex == "m" & Hist7$Dim == "M", "r"])


Hist7$rbin <- round(Hist7$r * 100) %/% 10 * 10

barplot(table(Hist7[Hist7$sex == "m" & Hist7$Dim == "L", "rbin"]),space=0)

varnames
categories <- rep(NA, length(varnames))
names(categories) <- varnames
categories[c("adl3","adl5","adlwalk","adldress",
				"adlbath","adleat","adlbed","adltoilet")]         <- "ADL"
categories[c("iadl3","iadl5","limwork","iadlmap",
				"iadltel","iadlmoney","iadlmeds",
				"iadlshop","iadlmeals")]                          <- "IADL"
categories[c("alcev","alcdays","alcdrinks","smokeev","smokecur")] <- "HB"
categories[c("bmi","back","mob","lgmus","grossmot","finemot")]    <- "FUNC"
categories[c("cc","bp","diab","cancer","lung","heart",
				"stroke","psych","arth")]                         <- "CHR"
categories[c("srm","pastmem","ss","c20b","namemo","namedmo",
				"nameyr","namedwk","namesci","namecac","namepres",
				"namevp","vocab","tm","dwr","twr","iwr")]         <- "COG"

categories[c("cesd","srh","cesddepr","cesdsleep","cesdhappy",
				"cesdlone","cesdsad","cesdgoing","cesdenjoy")]    <- "PSY"
categories[c("hosp","hospstays","hospnights","nh","nhstays",
				"nhnights","nhnow","doc","docvisits","hhc",
				"meds","surg","dent","shf" )]                     <- "HU"
Hist7$cat <- categories[Hist7$var]
unique(categories)
LM <- Hist7$sex == "m" & Hist7$Dim == "L"
AM <- Hist7$sex == "m" & Hist7$Dim == "A"
TM <- Hist7$sex == "m" & Hist7$Dim == "T"
MM <- Hist7$sex == "m" & Hist7$Dim == "M"

LF <- Hist7$sex == "f" & Hist7$Dim == "L"
AF <- Hist7$sex == "f" & Hist7$Dim == "A"
TF <- Hist7$sex == "f" & Hist7$Dim == "T"
MF <- Hist7$sex == "f" & Hist7$Dim == "M"


Hist7$rbin <- as.factor(Hist7$rbin)

rgbv <- function(x){
	rgb(x[1],x[2],x[3])
}
# from colorgorical
Palette <- apply(rbind(
				c(47,23,177),
				c(157,216,78), 
				c(248,73,182), 
				c(54,229,21), 
				c(148,20,131), 
				c(72,149,15), 
				c(188,28,250), 
				c(109,125,76), 
				c(198,151,244)) / 255, 1, rgbv)

cats <- c("ADL", "IADL","Health Behav.","Func. Lim.",
		"Chronic", "Cognitive", "Psychological", "Healthcare")
cats2 <- c("ADL", "IADL","HB","FUNC","CHR","COG","PSY","HU")


Hist7$R   <- Hist7$r * 100
Hist7$dim <- as.factor(Hist7$Dim )

pdf("Figures/HistFem.pdf",width=3,height=7)
histogram(~R | dim, data = Hist7[Hist7$sex == "f", ], col = gray(.4), 
		par.settings = list(strip.background=list(col=gray(.9))), layout=c(1,4),type="count",
		breaks = seq(0,100,by=10),index.cond=list(c(3,4,1,2)), 
		xlab = list(label="correlation coef * 100"),
		ylim=c(0,47))
dev.off()
pdf("Figures/HistMal.pdf",width=3,height=7)
histogram(~R | dim, data = Hist7[Hist7$sex == "m", ], col = gray(.4), 
		par.settings = list(strip.background=list(col=gray(.9))), layout=c(1,4),type="count",
		breaks = seq(0,100,by=10),index.cond=list(c(3,4,1,2)), 
		xlab = list(label="correlation coef * 100"),
		ylim=c(0,47))
dev.off()

length(varnames)

# end
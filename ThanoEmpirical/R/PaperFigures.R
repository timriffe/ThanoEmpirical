
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
source("R/SurfMap.R")
library(RColorBrewer)
library(lattice)
# this script contains just the figures used in the final submission to VYPR.
# many other figures are produced here and there in other R scripts, but are not included.


# ---------------------------------------------
# Figure 1
#
# LexisOrtho (I don't remember why ortho...)
# this is the TAL aka Lifecourse diagram
# ---------------------------------------------
gridcol <- gray(.6) # medium gray, not overwhelming.

pdf("Figures/LexisOrtho.pdf",width=4.5,height=4.5)
par(mai=c(.5,.5,.5,.5),xpd=TRUE)
plot(NULL, type="n",xlim=c(0,110),ylim=c(0,110), axes=FALSE,xlab="",ylab="", asp=1)
segments(rep(0,11),seq(10,111,by=10),seq(10,111,by=10),rep(0,11), col = gridcol)
#segments(111,0,111,1)
segments(0,0,110,0, col = gridcol)
segments(0,0,0,110, col = gridcol)
# horizontal:
segments(rep(0,10),seq(10,100,by=10),seq(100,10,by= -10),seq(10,100,by= 10), col = gridcol)
# vertical
segments(seq(10,100,by=10),rep(0,10),seq(10,100,by=10),seq(100,10,by= -10), col = gridcol)

# corner ticks:
segments(110,0,110,-1,col=gridcol)
segments(0,0,0,-1,col=gridcol)
segments(0,0,-1,0,col=gridcol)
segments(0,110,-1,110,col=gridcol)

text(55,-10,"chronological age\n(and calendar year)", cex = .8)
text(-10,55,"thanatological age",srt=90, cex = .8)

text(0,0,0,pos=1, cex = .8)
text(0,0,0,pos=2, cex = .8)
text(113,0,bquote(omega == 110),cex=.8,pos=1)
text(0,110,bquote(omega == 110),pos=2,cex=.8)
arrows(0,99,99-5,0+5,col="blue",lwd=2, lty = "41")
#arrows(0,55,55-15,0+15,col="blue",lwd=2)
segments(0,71,71,0,col="red",lwd=3)
points(71,0,pch=19,col="red")

text(80,70,"(B) incomplete lifeline", cex = .8)
segments(67,67,60,39,col="blue",lty=2)

text(60,80,"(A) complete lifeline", cex = .8)
segments(57,77,43,29,col="red",lty=2)


polygon(c(72,96,96-13,72),c(0,0,13,13),border = brewer.pal(8,"Greens")[8], lwd = 1, lty = 1,
		col = paste0(brewer.pal(8,"Greens")[5],"50"))
text(75,60,"(C) area studied for\n1915-1919 birth cohort", cex = .8, pos = 4)
segments(90,55,96-13,13,col=brewer.pal(8,"Greens")[8],lty=2)
dev.off()
# end figure 1

# ----------------------------
# Surface figures:

# load in smoothed surfaces
# object created in R/loessSmoothing.R
Results <- local(get(load("Data/LoessQuinquenal.Rdata")))

# ----------------------------
# T: thanatological age
# chrono: psych
grabber <- paste0("psych", "_", .7)
Surf <- Results[[grabber]][["Male"]]$Surf[, , as.character(1915)]
# 
pdf("Figures/Surf_Male_psych.pdf", width = 10, height = 6)
#dev.new(width = 10, height = 6)
SurfMap(Surf,
		napprox = 9,
		contour = TRUE,
		outline = FALSE,
		bg = TRUE,
		xlab = "chronological age", 
		ylab = "thanatological age")
dev.off()


# ----------------------------
# A: chronological age
grabber        <- paste0("back", "_", .7)
Surf           <- Results[[grabber]][["Female"]]$Surf[, , as.character(1915)]
Surf[Surf < 0] <- 0
pdf("Figures/Surf_Female_back.pdf", width = 10, height = 6)
#dev.new(width = 10, height = 6)
SurfMap(Surf,
		napprox = 9,
		contour = TRUE,
		outline = FALSE,
		bg = TRUE,
		xlab = "chronological age", 
		ylab = "thanatological age")
dev.off()

# ----------------------------
# L: lifespan:
grabber <- paste0("smoke_ev", "_", .7)
Surf    <- Results[[grabber]][["Female"]]$Surf[, , as.character(1915)]

pdf("Figures/Surf_Female_smoke_ev.pdf", width = 10, height = 6)
#dev.new(width = 10, height = 6)
SurfMap(Surf,
		napprox = 9,
		contour = TRUE,
		outline = FALSE,
		bg = TRUE,
		xlab = "chronological age", 
		ylab = "thanatological age")
dev.off()

#------------------------------------------------
# M: function of both
grabber <- paste0("bp", "_", .7)
Surf    <- Results[[grabber]][["Male"]]$Surf[, , as.character(1915)]

pdf("Figures/Surf_Male_bp.pdf", width = 10, height = 6)
#dev.new(width = 10, height = 6)
SurfMap(Surf,
		napprox = 9,
		contour = TRUE,
		outline = FALSE,
		bg = TRUE,
		xlab = "chronological age", 
		ylab = "thanatological age")
dev.off()

#------------------------------------------------
# just to make a point, fit chronological
# age pattern of psychological problems
# for 1915 cohort.
# It shows an increasing pattern over age, due to
# an interaction with lifespan.
#------------------------------------------------
# this code copied from
Dat      <- local(get(load("Data/Data_long.Rdata")))
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
# fit age-only within-cohort loess model
mod      <- stats::loess('psych~cafloor+Coh5',
		                data = Dat[Dat$sex == "m",],
						weight = p_wt2)
# predict for 1915 cohort, keep intervals
m1915se  <- predict(mod,
		           newdata = data.frame(cafloor = 72:95, Coh5 = 1915),
		           se = TRUE)

# produce figure 3
pdf("Figures/MalePsychChrono.pdf",height = 4.5,width=4.5)
par(mai = c(1.2,1,.5,.5))
plot(72:95, m1915se$fit, type = 'l', xaxs = "i", yaxs="i", ylim=c(0,.25),
  xlab = "chronological", ylab = "prevalence of psych problems",
  panel.first = list(
    rect(72,0,95,.25,col=gray(.9),border=NA),
    segments(72,seq(.05,.2,by=.05),95,seq(.05,.2,by=.05),col="white"),
    segments(seq(75,90,by=5),0,seq(75,90,by=5),.25,col="white")))
polygon(c(72:95,95:72),
  c(m1915se$fit - m1915se$se.fit * 2, rev(m1915se$fit + m1915se$se.fit * 2)), 
  col = "#00000040", border = NA)
dev.off()

# --------------------------------
# correlation histograms:

# File produced in Correlations.R
#
Results_r  <- local(get(load("Data/Correlations.Rdata")))
Hist7      <- Results_r[Results_r$span == "0.7" & Results_r$Cohort == 1915, ]
Hist7$rbin <- round(Hist7$r * 100) %/% 10 * 10
Hist7$rbin <- as.factor(Hist7$rbin)
Hist7$R    <- Hist7$r * 100
Hist7$Dim  <- as.factor(Hist7$Dim )

#
pdf("Figures/HistFem.pdf", width = 3, height = 7)
histogram(~R | Dim, 
		data = Hist7[Hist7$sex == "f", ], 
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

pdf("Figures/HistMal.pdf", width=  , height = 7)
histogram(~R | Dim, 
		data = Hist7[Hist7$sex == "m", ], 
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

# ----------------------------
# end
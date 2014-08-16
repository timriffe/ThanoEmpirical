# install.packages("data.table")
# install.packages("RColorBrewer")
library(data.table)
library(RColorBrewer)
# simple weighted mean function
wmean <- function(x,w){
    sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
}
getMeans <- function(.SD){
    with(.SD,cbind(adl3 = wmean(adl3,wt),
                    adl5 = wmean(adl5,wt),
                    iadl3 = wmean(iadl3,wt),
                    iadl5 = wmean(iadl5,wt),
                    cesd = wmean(cesd,wt)))
}
setwd("/home/triffe/git/ThanoEmpirical/ThanoEmpirical")

Dat <- local(get(load("Data/Data_long.Rdata")))

Dat <- data.table(Dat)
# a hack, to be removed when weights are properly figured out:
#Dat$wt[is.na(Dat$wt)] <- mean(Dat$wt, na.rm = TRUE)

# for binning purposes, akin to 'completed age'
Dat$tafloor <- floor(Dat$ta)

# I guess actual interview date could be some weeks prior to registered
# interview date? There are two negative thano ages at wave 4 otherwise, but
# still rather close. Likely died shortly after interview.
Dat$tafloor[Dat$tafloor < 0] <- 0

# greater than 15 we lose precision, cut off:
Dat <- Dat[Dat$tafloor <= 15, ]

# recode so factors 'back'
Dat$lim_work <- as.character(Dat$lim_work)
Dat$back     <- as.character(Dat$back)
Dat$srh      <- as.character(Dat$srh)

Dat$back <- ifelse(is.na(Dat$back), NA, ifelse(Dat$back == "1. yes",1,0))
Dat$srh[is.na(Dat$srh)] <- "NA"

recvec        <- c(0:4, NA)
names(recvec) <- sort(unique(Dat$srh))
Dat$srh       <- recvec[Dat$srh]
colnames(Dat) <- gsub("_","",colnames(Dat) )
Means         <- Dat[,list(
                     adl5 = wmean(adl5,pwt),
                     srh = wmean(srh,pwt),
                     back = wmean(back,pwt),
                     cesd = wmean(cesd,pwt)),by=list(sex,tafloor)]
Means         <- Means[with(Means,order(sex,tafloor)), ]

# females then males (16 rows each)
Means$sex     <- NULL
Means$tafloor <- NULL
Means         <- as.matrix(Means)

MeansScaled   <- scale(Means)

Colors        <- brewer.pal(ncol(Means),"Dark2")
Colors2 <- sapply(Colors, function(x){
            colorRampPalette(c(x,"white"), space = "Lab")(3)[2]
        })
ylim    <- range(pretty(MeansScaled,n=10))
ytix    <- unique(round(pretty(MeansScaled,n=10)))
ytix <- ytix[ytix >= min(ylim) & ytix <= max(ylim)]
mai     <- c(.5,.5,.5,1)
height  <- 4 + sum(mai[c(1,3)])
width   <- 4 + sum(mai[c(2,4)])
graphics.off()
#dev.new(height = height, width = width)
pdf("Figures/VariablePlots/ProposalFemales.pdf", height = height, width = width)
par(mai = mai, xpd = TRUE, xaxs = "i", yaxs = "i")
plot(NULL, type = 'n', axes = FALSE, xlim = c(0,15), ylim = ylim, xlab = "", ylab = "", panel.first =list(
                rect(0, ylim[1], 15, ylim[2], col = gray(.95), border = NA),
                segments(seq(0, 14, by = 2), ylim[1], seq(0, 14, by = 2), ylim[2], col = "white"),
                text(seq(0, 14, by = 2), ylim[1], seq(0, 14, by = 2), pos = 1, xpd = TRUE, cex = .8),
                segments(0, ytix, 15, ytix, col = "white"),
                text(0, ytix, ytix, pos = 2, cex = .8),
                text(7, ylim[1] - .5, "Years Left"),
                text(-2, ylim[2] + .4, "Mean value\n(centered & scaled)", pos = 4)))
matplot(0:15, MeansScaled[1:16, ], type = 'l', lty = 1, lwd = 1.5, col = Colors, add = TRUE)
legend(15,ylim[2],lty=1,lwd=1.5,col=Colors,legend=colnames(Means),xpd=TRUE,bty="n")
dev.off()
#dev.new(height = height, width = width)
pdf("Figures/VariablePlots/ProposalMales.pdf", height = height, width = width)
par(mai = mai, xpd = TRUE, xaxs = "i", yaxs = "i")
plot(NULL, type = 'n', axes = FALSE, xlim = c(0,15), ylim = ylim, xlab = "", ylab = "", panel.first =list(
                rect(0, ylim[1], 15, ylim[2], col = gray(.95), border = NA),
                segments(seq(0, 14, by = 2), ylim[1], seq(0, 14, by = 2), ylim[2], col = "white"),
                text(seq(0, 14, by = 2), ylim[1], seq(0, 14, by = 2), pos = 1, xpd = TRUE, cex = .8),
                segments(0, ytix, 15, ytix, col = "white"),
                text(0, ytix, ytix, pos = 2, cex = .8),
                text(7, ylim[1] - .5, "Years Left"),
                text(-2, ylim[2] + .4, "Mean value\n(centered & scaled)", pos = 4)))
matplot(0:15, MeansScaled[17:32, ], type = 'l', lty = 1, lwd = 1.5, col = Colors, add = TRUE)
legend(15,ylim[2],lty=1,lwd=1.5,col=Colors,legend=colnames(Means),xpd=TRUE,bty="n")
dev.off()

# png not used in the end. was for possible blog post, but nixed
#dev.new(height = height, width = width)
png("Figures/VariablePlots/ProposalFemales.png", height = height*90, width = width*90, res = 90)
par(mai = mai, xpd = TRUE, xaxs = "i", yaxs = "i")
plot(NULL, type = 'n', axes = FALSE, xlim = c(0,15), ylim = ylim, xlab = "", ylab = "", panel.first =list(
                rect(0, ylim[1], 15, ylim[2], col = gray(.95), border = NA),
                segments(seq(0, 14, by = 2), ylim[1], seq(0, 14, by = 2), ylim[2], col = "white"),
                text(seq(0, 14, by = 2), ylim[1], seq(0, 14, by = 2), pos = 1, xpd = TRUE, cex = .8),
                segments(0, ytix, 15, ytix, col = "white"),
                text(0, ytix, ytix, pos = 2, cex = .8),
                text(7, ylim[1] - .5, "Years Left"),
                text(-2, ylim[2] + .4, "Mean value\n(centered & scaled)", pos = 4)))
matplot(0:15, MeansScaled[1:16, ], type = 'l', lty = 1, lwd = 1.5, col = Colors, add = TRUE)
legend(15,ylim[2],lty=1,lwd=1.5,col=Colors,legend=colnames(Means),xpd=TRUE,bty="n")
dev.off()
#dev.new(height = height, width = width)
png("Figures/VariablePlots/ProposalMales.png", height = height*90, width = width*90, res = 90)
par(mai = mai, xpd = TRUE, xaxs = "i", yaxs = "i")
plot(NULL, type = 'n', axes = FALSE, xlim = c(0,15), ylim = ylim, xlab = "", ylab = "", panel.first =list(
                rect(0, ylim[1], 15, ylim[2], col = gray(.95), border = NA),
                segments(seq(0, 14, by = 2), ylim[1], seq(0, 14, by = 2), ylim[2], col = "white"),
                text(seq(0, 14, by = 2), ylim[1], seq(0, 14, by = 2), pos = 1, xpd = TRUE, cex = .8),
                segments(0, ytix, 15, ytix, col = "white"),
                text(0, ytix, ytix, pos = 2, cex = .8),
                text(7, ylim[1] - .5, "Years Left"),
                text(-2, ylim[2] + .4, "Mean value\n(centered & scaled)", pos = 4)))
matplot(0:15, MeansScaled[17:32, ], type = 'l', lty = 1, lwd = 1.5, col = Colors, add = TRUE)
legend(15,ylim[2],lty=1,lwd=1.5,col=Colors,legend=colnames(Means),xpd=TRUE,bty="n")
dev.off()
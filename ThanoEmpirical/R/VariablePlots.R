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
Dat$wt[is.na(Dat$wt)] <- mean(Dat$wt, na.rm = TRUE)

# for binning purposes, akin to 'completed age'
Dat$tafloor <- floor(Dat$ta)

# I guess actual interview date could be some weeks prior to registered
# interview date? There are two negative thano ages at wave 4 otherwise, but
# still rather close. Likely died shortly after interview.
Dat$tafloor[Dat$tafloor < 0] <- 0

# greater than 15 we lose precision, cut off:
Dat <- Dat[Dat$tafloor <= 15, ]

Means       <- Dat[,list(
                adl3 = wmean(adl3,wt),
                adl5 = wmean(adl5,wt),
                iadl3 = wmean(iadl3,wt),
                iadl5 = wmean(iadl5,wt),
                cesd = wmean(cesd,wt)),by=list(sex,tafloor)]
Means <- Means[with(Means,order(sex,tafloor)), ]

# females then males (16 rows each)
Means$sex     <- NULL
Means$tafloor <- NULL
Means         <- as.matrix(Means)

MeansScaled   <- scale(Means)
display.brewer.all()
Colors        <- brewer.pal(ncol(Means),"Dark2")
Colors2 <- sapply(Colors, function(x){
            colorRampPalette(c(x,"white"), space = "Lab")(3)[2]
        })
ylim    <- range(pretty(MeansScaled,n=10))
ytix    <- unique(round(pretty(MeansScaled,n=10)))
mai     <- c(.3,.3,.3,1.5)
height  <- 4 + sum(mai[c(1,3)])
width   <- 4 + sum(mai[c(2,4)])
par(mai = mai)

plot(NULL, type = 'n', axes = FALSE, xlim = c(0,15), ylim = ylim, xlab = "", ylab = "", panel.first =list(
                rect(0, ylim[1], 15, ylim[2], col = gray(.95), border = NA),
                segments(seq(0, 14, by = 2), ylim[1], seq(0, 14, by = 2), ylim[2], col = "white"),
                text(seq(0, 14, by = 2), ylim[1], seq(0, 14, by = 2), pos = 1, xpd = TRUE, cex = .8),
                segments(0, ytix, 15, ytix, col = "white"),
                text(0, ytix, ytix, pos = 2, cex = .8)))
matplot(0:15, MeansScaled[1:16, ], type = 'l', lty = 1, lwd = 1.5, col = Colors, add = TRUE)
legend(0,-2.5,lty=1,lwd=1.5,col=Colors,legend=colnames(Means),xpd=TRUE,bty="n")


par(mai = c(.3,.3,.3,1.5))
ylim <- range(pretty(MeansScaled,n=10))
ytix <- unique(round(pretty(MeansScaled,n=10)))
plot(NULL, type = 'n', axes = FALSE, xlim = c(0,15), ylim = ylim, xlab = "", ylab = "", panel.first =list(
                rect(0, ylim[1], 15, ylim[2], col = gray(.95), border = NA),
                segments(seq(0, 14, by = 2), ylim[1], seq(0, 14, by = 2), ylim[2], col = "white"),
                text(seq(0, 14, by = 2), ylim[1], seq(0, 14, by = 2), pos = 1, xpd = TRUE, cex = .8),
                segments(0, ytix, 15, ytix, col = "white"),
                text(0, ytix, ytix, pos = 2, cex = .8)))
matplot(0:15, MeansScaled[17:32, ], type = 'l', lty = 1, lwd = 1.5, col = Colors, add = TRUE)
legend(0,-2.5,lty=1,lwd=1.5,col=Colors,legend=colnames(Means),xpd=TRUE,bty="n")



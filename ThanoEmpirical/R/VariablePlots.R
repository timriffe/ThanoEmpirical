setwd("/home/tim/git/ThanoEmpirical/ThanoEmpirical")
library(reshape2)
library(RColorBrewer)

# ------------------------------------------------------        
SurfaceList <- local(get(load("Data/SurfaceList.Rdata")))
# ------------------------------------------------------        
varnames <- names(SurfaceList)


library(LexisUtils)
pdf("Figures/VariablePlots/SurfacesMales.pdf", width = 5, height = 4)
for (varname in varnames){
   
            LexisMap(SurfaceList[[varname]]$Male, log = FALSE, 
                    xlab = "Years Lived", 
                    ylab = "Years Left",
                    main = paste("Males",varname),
                    contour = TRUE, 
                    LexRef = FALSE
            )
    
}

dev.off()

pdf("Figures/VariablePlots/SurfacesFemales.pdf", width = 5, height = 4)
for (varname in varnames){
    
    LexisMap(SurfaceList[[varname]]$Female, log = FALSE, 
            xlab = "Years Lived", 
            ylab = "Years Left",
            main = paste("Females",varname),
            contour = TRUE, 
            LexRef = FALSE
    )
    
}
dev.off()







library(reshape2)
Dat <- data.frame(Dat)
#acast(Dat[Dat$sex == "m",],cafloor3~tafloor3, value.var = "srh", 
#        fun.aggregate = wmean, w = Dat$p_wt2[Dat$sex=="m"])
#
#acast(Dat[Dat$sex == "m",],list("cafloor2","tafloor2"), value.var = "srh", 
#        fun.aggregate = wmean, w = "p_wt2")
#

makeMatrix <- function(Dat,sex,variable="srh",weight="p_wt2"){
    # waiting on SO response
    acast(Dat[Dat$sex == sex, ],cafloor3~tafloor3, value.var = "srh", 
            fun.aggregate = wmean, w = weight)
}
acast(Dat2,cafloor2~tafloor2, value.var = c("srh","p_wt2"),
        fun.aggregate = wmean)

wmean(Dat2$srh,Dat2$p_wt2)

image(acast(Dat[Dat$sex == "m", ],cafloor2~tafloor2, value.var = "srh", 
        fun.aggregate = mean, na.rm=TRUE)
)












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
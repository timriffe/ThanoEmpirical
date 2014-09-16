
# install.packages("data.table")
# install.packages("RColorBrewer")
library(data.table)
library(reshape2)
library(RColorBrewer)
# simple weighted mean function
wmean <- function(x,w=rep(1,length(x))){
    if (length(x)==0){
        return(NA)
    }
    sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
}
getMeans <- function(.SD){
    with(.SD,cbind(adl3 = wmean(adl3,wt),
                    adl5 = wmean(adl5,wt),
                    iadl3 = wmean(iadl3,wt),
                    iadl5 = wmean(iadl5,wt),
                    cesd = wmean(cesd,wt)))
}
setwd("/home/tim/git/ThanoEmpirical/ThanoEmpirical")

Dat <- local(get(load("Data/Data_long.Rdata")))

Dat <- data.table(Dat)
# a hack, to be removed when weights are properly figured out:
#Dat$wt[is.na(Dat$wt)] <- mean(Dat$wt, na.rm = TRUE)

# for binning purposes, akin to 'completed age'
Dat$tafloor <- floor(Dat$ta)
Dat$cafloor <- floor(Dat$ca)
# I guess actual interview date could be some weeks prior to registered
# interview date? There are two negative thano ages at wave 4 otherwise, but
# still rather close. Likely died shortly after interview.
Dat$tafloor[Dat$tafloor < 0] <- 0

# greater than 15 we lose precision, cut off:
Dat <- Dat[Dat$tafloor <= 15, ]
Dat <- Dat[Dat$cafloor >= 60, ]


# bin ages
Dat$cafloor2 <- Dat$cafloor - Dat$cafloor %% 2
Dat$tafloor2 <- Dat$tafloor - Dat$tafloor %% 2

Dat$cafloor3 <- Dat$cafloor - Dat$cafloor %% 3
Dat$tafloor3 <- Dat$tafloor - Dat$tafloor %% 3

# recode so factors 'back'
#Dat$lim_work <- as.character(Dat$lim_work)
#Dat$back     <- as.character(Dat$back)
#Dat$srh      <- as.character(Dat$srh)
#
#Dat$back <- ifelse(is.na(Dat$back), NA, ifelse(Dat$back == "1. yes",1,0))
#Dat$srh[is.na(Dat$srh)] <- "NA"
#
#recvec        <- c(0:4, NA)
#names(recvec) <- sort(unique(Dat$srh))
#Dat$srh       <- recvec[Dat$srh]
#colnames(Dat) <- gsub("_","",colnames(Dat) )
#Dat <- data.table(Dat)

varnames      <- c(
       "iadl3_", "iadl5_", "cesd", "lim_work", "srh", 
        "bmi", "back",  "hosp", "hosp_stays", "hosp_nights", "nh", 
        "nh_stays", "nh_nights", "nh_now", "nh_mo", "nh_yr", "nh_days", 
        "doc", "doc_visits", "hhc", "meds", "surg", "dent", "shf", "adl_walk", 
        "adl_dress", "adl_bath", "adl_eat", "adl_bed", "adl_toilet", 
        "iadl_map", "iadl_tel", "iadl_money", "iadl_meds", "iadl_shop", 
        "iadl_meals", "mob", "lg_mus", "gross_mot", "fine_mot", "bp", 
        "diab", "cancer", "lung", "heart", "stroke", "psych", "arth", 
        "cc", "vig_freq", "lt_freq", "mod_freq", "alc_ev", "alc_days", 
        "alc_drinks", "smoke_ev", "smoke_cur", "cesd_depr", "cesd_eff", 
        "cesd_sleep", "cesd_happy", "cesd_lone", "cesd_sad", "cesd_going", 
        "cesd_enjoy", "med_exp", "iadl_calc", "ulc", "vig", "lt")     
varnames <- varnames[varnames %in% colnames(Dat)]

SurfaceList <- list()
Dat <- data.frame(Dat)
#Dat$mod_freq
for (varname in varnames){
          
            Dati <- Dat[, c("sex","tafloor2","cafloor2","p_wt2",varname)]
            colnames(Dati)[5] <- "V1"
            Mean <- 
                    data.table(Dati)[,  list(V1 = wmean(V1,p_wt2)),
                               by = list(sex,tafloor2,cafloor2)]
            Mean <- data.frame(Mean)   
   
            MeanM <- acast(Mean[Mean$sex == "m", ],tafloor2~cafloor2,value.var="V1" )
            MeanF <- acast(Mean[Mean$sex == "f", ],tafloor2~cafloor2,value.var="V1" )
            
            #setnames(Dat,cols="V1",value=varname)
            
            SurfaceList[[varname]] <- list(Male = MeanM,Female = MeanF)
        }
        

        
Meta <- readLines("/home/tim/git/ThanoEmpirical/ThanoEmpirical/Data/thanos_wide_v2_0.txt")        


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
args(LexisMap)
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
args(LexisMap)






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
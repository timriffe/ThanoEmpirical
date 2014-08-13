
# this make be junk code as of current version. Just a scratch pad is all.
adl5        <- paste0("adl5_", 2:10)
ta          <- paste0("ta_", 2:10)
sex         <- "m"
ADL5        <- as.matrix(Dat[Dat$sex == sex, adl5])
TA          <- as.matrix(Dat[Dat$sex == sex, ta])
ADL5        <- cbind(ADL5, NA)
TA          <- cbind(TA, NA)
dim(ADL5)   <- dim(TA) <- NULL

plot(TA, jitter(ADL5),type='l',col = "#00000010",xlab = "years left",ylab = "adl5" )


wmean <- function(x,w){
    sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
}

colnames(Dat)
Dlong <- list()
for (i in 1:length(adl5)){
    cols <- c("wt","sex",adl5[i],ta[i])
    Dati <- Dat[,cols]
    colnames(Dati) <- c("wt","sex","adl5","ta")
    Dlong[[i]] <- Dati
}
Dlong         <- do.call(rbind, Dlong)
Dlong$tafloor <- floor(Dlong$ta)

Dlong         <- Dlong[!is.na(Dlong$adl5),]
Dlong         <- Dlong[Dlong$tafloor >= 0 & Dlong$tafloor < 16,]
library(data.table)
Dlong         <- data.table(Dlong)

Means         <- Dlong[,wmean(adl5,wt),by=list(sex,tafloor)]

library(reshape2)
MADL5 <- acast(Means,tafloor~sex, value.var = "V1")

png("Figures/PatternADL5.png")
matplot(0:15, MADL5, type = 'l', lty = 1, ylab = "adl5", xlab = "Years Left", col = c("red","blue"),main = "ADL5 by remaining years of life and sex, HRS waves 2-10")
legend("topright",lty=1,col=c("red","blue"),legend=c("female","male"),bty="n")
dev.off()

# this doesn't render math or pdf figures well, decided not to do it.
#setwd("/home/triffe/git/ThanoEmpirical/ThanoEmpirical")
#system("latex2html ThanoEmpirical.tex")

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


# need modal age at death from the 1915-1919 cohort...

library(DemogBerkeley)
cMx <- readHMDweb("USA","cMx_1x1",username=us,password=pw)

m1915 <- cMx$Male[cMx$Year == 1915]
m1919 <- cMx$Male[cMx$Year == 1919]

f1915 <- cMx$Female[cMx$Year == 1915]
f1919 <- cMx$Female[cMx$Year == 1919]

ages <- 0:110
keep1915 <- !is.na(m1915)
a1915 <- ages[keep1915]
m1915 <- m1915[keep1915]
f1915 <- f1915[keep1915]

keep1919 <- !is.na(m1919)
a1919 <- ages[keep1919]
m1919 <- m1919[keep1919]
f1919 <- f1919[keep1919]

dxm1915 <- -diff(c(1,exp(-cumsum(m1915))))
dxf1915 <- -diff(c(1,exp(-cumsum(f1915))))
dxm1919 <- -diff(c(1,exp(-cumsum(m1919))))
dxf1919 <- -diff(c(1,exp(-cumsum(f1919))))

a1915[which.max(dxm1915)]
a1919[which.max(dxm1919)]

a1915[which.max(dxf1915)]
a1919[which.max(dxf1919)]
plot(a1915,-diff(c(1,exp(-cumsum(m1915)))), type = 'l', col = "blue", ylim=c(0,.035))
lines(a1915,-diff(c(1,exp(-cumsum(f1915)))),  col = "magenta")
lines(a1919,-diff(c(1,exp(-cumsum(m1919)))),  col = "blue", lty = 2)
lines(a1919,-diff(c(1,exp(-cumsum(f1919)))),  col = "magenta", lty = 2)





fpp <- read.csv("FranklinIndiaPrimaPlusRG.csv", stringsAsFactors=FALSE)
ipa <- read.csv("ICICIPruEqArbRG.csv", stringsAsFactors=FALSE)

ipa$Date <- as.Date(ipa$Date, "%d-%b-%y")
fpp$Date <- as.Date(fpp$Date, "%d-%b-%y")

ipaNoZero = ipa[ipa$NAV != 0, ]
fppNoZero = fpp[fpp$NAV != 0, ]

ipaNoDup = unique(ipaNoZero[,])
fppNoDup = unique(fppNoZero[,])

datesDf = seq.Date(as.Date("2007-01-01"), as.Date("2015-09-22"), by="days")

zeros = rep(0,length(datesDf))
cdf = data.frame(Date=datesDf, ipa=zeros, fpp=zeros)

skipMode = FALSE
for (d in cdf$Date){
    if(d %in% ipaNoDup$Date){
        curNav = ipaNoDup[ipaNoDup$Date == d, ]$NAV
        cdf[cdf$Date == d, ]$ipa = curNav
        if (skipMode == TRUE){
            endSet = as.Date(d - 1, origin="1970-01-01")
            skipDates = seq.Date(startSet, endSet, by="days")
            for (x in skipDates){
                cdf[cdf$Date == x, ]$ipa = curNav
            }
            skipMode = FALSE
        }
    } else{
        if(skipMode == FALSE){
            startSet = as.Date(d, origin="1970-01-01")
            skipMode = TRUE
        }
    }
} 


skipMode = FALSE
for (d in cdf$Date){
    if(d %in% fppNoDup$Date){
        curNav = fppNoDup[fppNoDup$Date == d, ]$NAV
        cdf[cdf$Date == d, ]$fpp = curNav
        if (skipMode == TRUE){
            endSet = as.Date(d - 1, origin="1970-01-01")
            skipDates = seq.Date(startSet, endSet, by="days")
            for (x in skipDates){
                cdf[cdf$Date == x, ]$fpp = curNav
            }
            skipMode = FALSE
        }
    }
    else{
        if(skipMode == FALSE){
            startSet = as.Date(d, origin="1970-01-01")
            skipMode = TRUE
        }
    }
} 

invStartDate = as.Date("2007-01-01")
gapYears = 1
stpYears = 1

compareFn = function(invStartDate, cdf, gapYears, stpYears){
    invAmt = 3200000
    stpMonths = stpYears * 12

    arbInvDate = invStartDate
    invNAV = cdf[cdf$Date == arbInvDate, ]$ipa
    arbUnits = invAmt / invNAV

    fppNAV = cdf[cdf$Date == arbInvDate, ]$fpp
    fppUnits = invAmt / fppNAV

    arbRedDate = tail(seq.Date(invStartDate, by="year", 
        length.out = gapYears + 1), 1)
    redNAV = cdf[cdf$Date == arbRedDate, ]$ipa
    redAmt = arbUnits * redNAV

    amountSTP = redAmt / stpMonths
    stpDates = seq.Date(arbRedDate, by = "month", length.out = stpMonths)

    gwtUnits = 0
    for (x in stpDates){
        swpNAV = cdf[cdf$Date == x, ]$ipa
        sipNAV = cdf[cdf$Date == x, ]$fpp
        
        swpUnits = amountSTP / swpNAV
        sipUnits = amountSTP / sipNAV
        
        arbUnits = arbUnits - swpUnits
        gwtUnits = gwtUnits + sipUnits
    }

    redeemDate = tail(seq.Date(invStartDate, by="year", 
        length.out = gapYears + stpYears + 1), 1)
        
    redArbNAV = cdf[cdf$Date == redeemDate, ]$ipa
    redGwtNAV = cdf[cdf$Date == redeemDate, ]$fpp

    arbRedeem = arbUnits * redArbNAV
    gwtRedeem = gwtUnits * redGwtNAV

    redStripMode = arbRedeem + gwtRedeem
    redFullMode = fppUnits * redGwtNAV
    
    netArbPlus = redStripMode - redFullMode
    ratArbPlus = netArbPlus / invAmt
    netRatio = ((1 + ratArbPlus) ^ (1 / (gapYears + stpYears))) - 1
    return(c(redStripMode - invAmt, redFullMode - invAmt))
}


# A = P (1 + r/100)^ n 
# int = A - P = P(1 + r/100)^n - P
# int/P = A/P - 1 = (1+ r/100)^n - 1 

pZeroTwo=sapply(seq.Date(invStartDate, by="month", length.out=81), compareFn, cdf, 0, 2)
    

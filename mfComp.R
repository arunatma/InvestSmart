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
    return(c(redStripMode - invAmt, redFullMode - invAmt, netArbPlus, netRatio))
}


# A = P (1 + r/100)^ n 
# int = A - P = P(1 + r/100)^n - P
# int/P = A/P - 1 = (1+ r/100)^n - 1 

pZeroOne = sapply(seq.Date(invStartDate, by="month", length.out=93), compareFn, 
	cdf, 0, 1)
pZeroTwo = sapply(seq.Date(invStartDate, by="month", length.out=81), compareFn, 
	cdf, 0, 2)
pOneOne = sapply(seq.Date(invStartDate, by="month", length.out=81), compareFn, 
	cdf, 1, 1)
pOneTwo = sapply(seq.Date(invStartDate, by="month", length.out=69), compareFn, 
	cdf, 1, 2)
pOneThree = sapply(seq.Date(invStartDate, by="month", length.out=57), compareFn, 
	cdf, 1, 3) 
	
pTwoOne = sapply(seq.Date(invStartDate, by="month", length.out=69), compareFn, 
	cdf, 2, 1)
pTwoTwo = sapply(seq.Date(invStartDate, by="month", length.out=57), compareFn, 
	cdf, 2, 2)
pTwoThree = sapply(seq.Date(invStartDate, by="month", length.out=45), compareFn, 
	cdf, 2, 3) 
	
stripProfit01 = pZeroOne[1,]
fullProfit01 = pZeroOne[2,]
netProfit01 = pZeroOne[3,]
netPct01 = pZeroOne[4,]

stripProfit02 = pZeroTwo[1,]
fullProfit02 = pZeroTwo[2,]
netProfit02 = pZeroTwo[3,]
netPct02 = pZeroTwo[4,]

stripProfit11 = pOneOne[1,]
fullProfit11 = pOneOne[2,]
netProfit11 = pOneOne[3,]
netPct11 = pOneOne[4,]

stripProfit12 = pOneTwo[1,]
fullProfit12 = pOneTwo[2,]
netProfit12 = pOneTwo[3,]
netPct12 = pOneTwo[4,]

stripProfit13 = pOneThree[1,]
fullProfit13 = pOneThree[2,]
netProfit13 = pOneThree[3,]
netPct13 = pOneThree[4,]

stripProfit21 = pTwoOne[1,]
fullProfit21 = pTwoOne[2,]
netProfit21 = pTwoOne[3,]
netPct21 = pTwoOne[4,]

stripProfit22 = pTwoTwo[1,]
fullProfit22 = pTwoTwo[2,]
netProfit22 = pTwoTwo[3,]
netPct22 = pTwoTwo[4,]

stripProfit23 = pTwoThree[1,]
fullProfit23 = pTwoThree[2,]
netProfit23 = pTwoThree[3,]
netPct23 = pTwoThree[4,]


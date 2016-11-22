library(data.table)
#-------------------------------------------------------------------------------
#                           Data Load
#-------------------------------------------------------------------------------
longMf      <- read.csv("FranklinIndiaPrimaPlusRG.csv", stringsAsFactors=FALSE)
#shortMf     <- read.csv("ICICIPruEqArbRG.csv", stringsAsFactors=FALSE)
shortMf     <- read.csv("TataBalancedRG.csv", stringsAsFactors=FALSE)
startDate   <- as.Date("2007-01-01")
endDate     <- as.Date("2016-10-20")

#-------------------------------------------------------------------------------
#                           Data Preparation
#-------------------------------------------------------------------------------

# 1. Conversion to Date Format
shortMf$Date <- as.Date(shortMf$Date, "%d-%b-%y")
longMf$Date <- as.Date(longMf$Date, "%d-%b-%y")

# 2. Remove all rows where NAV is zero
shortMfNoZero = shortMf[shortMf$NAV != 0, ]
longMfNoZero = longMf[longMf$NAV != 0, ]

# 3. Remove all duplicate rows 
shortMfNoDup = unique(shortMfNoZero[,])
longMfNoDup = unique(longMfNoZero[,])

# 4. Generate all individual dates from start to end dates.
datesDf = seq.Date(startDate, endDate, by="days")

# 5. Create a vector containing the total number of days.
zeros = rep(0, length(datesDf))

# 6. Make a temporary data frame with Date, shortMf and longMf as columns
#    Date column will contain the dates, while shortMf and longMf are initally
#    assigned as zeros 
cdf = data.frame(Date=datesDf, shortMf=zeros, longMf=zeros)

#-------------------------------------------------------------------------------
# fillNavs:  
#       Assigns the NAV for the holidays.  Ideally the NAV for a 
#       holiday should be the NAV available on the next working day.
#-------------------------------------------------------------------------------
fillNavs <- function (combinedDf, indivDf, colName){
    colIdx <- which(names(combinedDf) == colName)
    skipMode = FALSE
    for (d in combinedDf$Date){
        if(d %in% indivDf$Date){
            curNav = indivDf[indivDf$Date == d, ]$NAV[1]
            combinedDf[combinedDf$Date == d, ][colIdx] = curNav
            if (skipMode == TRUE){
                endSet = as.Date(d - 1, origin="1970-01-01")
                skipDates = seq.Date(startSet, endSet, by="days")
                for (x in skipDates){
                    combinedDf[combinedDf$Date == x, ][colIdx] = curNav
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
    return(combinedDf)
}

emptyCdf = data.frame(Date=datesDf, shortMf=zeros, longMf=zeros)
shortFilledCdf <- fillNavs(emptyCdf, shortMfNoDup, "shortMf")
cdf <- fillNavs(shortFilledCdf, longMfNoDup, "longMf")
cdf <- data.table(cdf)

compareFn = function(invStartDate, cdf, gapYears, stpYears){
    invAmt = 3200000
    stpMonths = stpYears * 12

    arbInvDate = invStartDate
    invNAV = cdf[cdf$Date == arbInvDate, ]$shortMf
    arbUnits = invAmt / invNAV

    longMfNAV = cdf[cdf$Date == arbInvDate, ]$longMf
    longMfUnits = invAmt / longMfNAV

    arbRedDate = tail(seq.Date(invStartDate, by="year", 
        length.out = gapYears + 1), 1)
    redNAV = cdf[cdf$Date == arbRedDate, ]$shortMf
    redAmt = arbUnits * redNAV

    amountSTP = redAmt / stpMonths
    stpDates = seq.Date(arbRedDate, by = "month", length.out = stpMonths)

    gwtUnits = 0
    for (x in stpDates){
        swpNAV = cdf[cdf$Date == x, ]$shortMf
        sipNAV = cdf[cdf$Date == x, ]$longMf
        
        swpUnits = amountSTP / swpNAV
        sipUnits = amountSTP / sipNAV
        
        arbUnits = arbUnits - swpUnits
        gwtUnits = gwtUnits + sipUnits
    }

    redeemDate = tail(seq.Date(invStartDate, by="year", 
        length.out = gapYears + stpYears + 1), 1)
        
    redArbNAV = cdf[cdf$Date == redeemDate, ]$shortMf
    redGwtNAV = cdf[cdf$Date == redeemDate, ]$longMf

    arbRedeem = arbUnits * redArbNAV
    gwtRedeem = gwtUnits * redGwtNAV

    redStripMode = arbRedeem + gwtRedeem
    redFullMode = longMfUnits * redGwtNAV
    
    netArbPlus = redStripMode - redFullMode
    ratArbPlus = netArbPlus / invAmt
    netRatio = ((1 + ratArbPlus) ^ (1 / (gapYears + stpYears))) - 1
    return(c(redStripMode - invAmt, redFullMode - invAmt, netArbPlus, netRatio))
}


# A = P (1 + r/100)^ n 
# int = A - P = P(1 + r/100)^n - P
# int/P = A/P - 1 = (1+ r/100)^n - 1 

invStartDate = as.Date("2007-01-01")

pZeroOne = sapply(seq.Date(invStartDate, by="month", length.out=106), compareFn, cdf, 0, 1)
pZeroTwo = sapply(seq.Date(invStartDate, by="month", length.out=94), compareFn,	cdf, 0, 2)
pOneOne = sapply(seq.Date(invStartDate, by="month", length.out=94), compareFn, cdf, 1, 1)
pOneTwo = sapply(seq.Date(invStartDate, by="month", length.out=82), compareFn, cdf, 1, 2)
pOneThree = sapply(seq.Date(invStartDate, by="month", length.out=70), compareFn, cdf, 1, 3) 
pOneFour = sapply(seq.Date(invStartDate, by="month", length.out=46), compareFn, cdf, 1, 4) 
	
pTwoOne = sapply(seq.Date(invStartDate, by="month", length.out=82), compareFn, cdf, 2, 1)
pTwoTwo = sapply(seq.Date(invStartDate, by="month", length.out=70), compareFn, cdf, 2, 2)
pTwoThree = sapply(seq.Date(invStartDate, by="month", length.out=58), compareFn, cdf, 2, 3) 
pTwoFour = sapply(seq.Date(invStartDate, by="month", length.out=46), compareFn, cdf, 2, 4) 
	
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

stripProfit14 = pOneFour[1,]
fullProfit14 = pOneFour[2,]
netProfit14 = pOneFour[3,]
netPct14 = pOneFour[4,]

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

stripProfit24 = pTwoFour[1,]
fullProfit24 = pTwoFour[2,]
netProfit24 = pTwoFour[3,]
netPct24 = pTwoFour[4,]

summary(netPct01)
summary(netPct02)
summary(netPct11)
summary(netPct12)
summary(netPct21)
summary(netPct13)
summary(netPct22)
summary(netPct14)
summary(netPct23)
summary(netPct24)

summary(netProfit01)
summary(netProfit02)
summary(netProfit11)
summary(netProfit12)
summary(netProfit21)
summary(netProfit13)
summary(netProfit22)
summary(netProfit14)
summary(netProfit23)
summary(netProfit24)

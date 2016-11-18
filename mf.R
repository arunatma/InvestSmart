#-------------------------------------------------------------------------------
# mf.R 
# Arunram Atmacharan 
# Functions that perform AMFI download, XIRR calculation
#-------------------------------------------------------------------------------

# Load Required libraries
library(XML)
library(data.table)    
    
# Function that returns another function, composing two functions.
compose <- function(f, g){
    return(function(...) f(g(...)))
}

# Higher order function by combining elementary functions
toDouble = compose(as.double, as.character)

# Safe Division
divBy <- function (b){
    if (b == 0) return (0)
    else return (1 / b)
}

# To find whether a given vector is of zero length. 
isEmpty <- function(x) {
    return(length(x) == 0)
}

# getNav: Returns NAV for a given date.
# Inputs: Given Date, Data Table Containing "Date" and "NetAssetValue" columns.
getNav <- function(inDate, navTable){
    minDate = navTable[1, Date]
    
    maxRow = nrow(navTable)
    maxDate = navTable[maxRow, Date]
    
    curDate = inDate
    # Return a Zero NAV for any date prior to earliest date available
    if (curDate < minDate){
        return(0)
        break
    }
    
    dateVector = navTable$Date
    rowNum = which(dateVector == curDate)
    while (isEmpty(rowNum)){
        # Return a Zero NAV for any date later than the latest date available
        if (curDate > maxDate){
            return(0)
            break
        }
        # If NAV is not available on a particular date, get the next 
        # available value 
        curDate = curDate + 1
        rowNum = which(dateVector == curDate)
    }
    
    # If there are duplicate entries of date in the data frame, take the 
    # first available date 
    return(navTable[rowNum[1], NetAssetValue])
}

getFromAmfi <- function(statURL){
    # http://portal.amfiindia.com/NavHistoryReport_Frm.aspx
    pageData <- readHTMLTable(statURL)
    namesVec <- as.vector(unlist(pageData[[4]][4,]))
    colNames <- gsub(" ", "", namesVec)
    navTable <- data.table(pageData[[4]][-c(1:4), ])
    
    # To start the row numbers from 1, instead of 5 
    row.names(navTable) <- NULL
    # Name the columns appropriately
    names(navTable) <- colNames
    
    # Keep only the NAV and Date Columns
    navTable[, NetAssetValue := toDouble(NetAssetValue)]
    navTable[, Date := as.Date(as.character(Date), "%d-%b-%Y")]
    navTable[, RepurchasePrice := NULL]
    navTable[, SalePrice := NULL]
    
    return (navTable)
}

# Calculate the future value given cash-flows(p), rate(r) and days(n)
getNPV <- function(r, p, n){
    sum(p * (1 + r) ^ (n/365)) 
}

# Calculate XIRR given vectors of cash flows and dates.
xirr <- function(cfs, dates) { 
    lastDate <- tail(dates, 1)
    numDays = as.double(lastDate - dates)
    # uniroot searches the interval from lower to upper for a root (i.e., zero) 
    # of the function f with respect to its first argument. 
    return(uniroot(getNPV, c(-1, 10), p = cfs, n = numDays)$root) 
}
    
getSipIrr <- function(sip, startDate, numYears, freq, navTable){
    endDate = startDate + numYears * 365
    redeemDate = endDate
    invDates <- seq.Date(startDate, endDate, freq)
    navs <- sapply(invDates, getNav, navTable = navTable)
    sips = rep(sip, length(navs))
    invUnits <- sips * unlist(Map(divBy, navs))
    zeroAdjSips = navs * invUnits
    redeemNav <- getNav(redeemDate, navTable)
    redeemUnits <- sum(invUnits)
    redeemValue <- redeemUnits * redeemNav

    cfs <- c(zeroAdjSips, -redeemValue)
    allDates <- c(invDates, redeemDate)
    irrValue = xirr(cfs, allDates)
    return(irrValue)
}


dlStartDate <- "03-Apr-2006"
dlEndDate <- "17-Aug-2016"
mfCode <- "28"
schemeCode <- "100669"
statURL <- paste0(
    "http://portal.amfiindia.com/NavHistoryReport_Rpt_Po.aspx?rpt=1&frmdate=", 
    dlStartDate,"&todate=", dlEndDate,"&mf=", mfCode, "&scm=", schemeCode)    
navTable <- getFromAmfi(statURL)

startStr = "03-Apr-2012"
startDate = as.Date(startStr, "%d-%b-%Y") 
numYears = 7
freq = "week"  #month #day #quarter #year
sip = 500
getSipIrr(sip, startDate, numYears, freq, navTable)

startDates <- seq.Date(startDate, startDate + 365 * 6, "day")
irr4yr <- sapply(startDates, getSipIrr, sip=sip, numYears=4, 
    freq=freq, navTable=navTable)

startDates <- seq.Date(startDate, startDate + 365 * 5, "day")
irr5yr <- sapply(startDates, getSipIrr, sip=sip, numYears=5, 
    freq=freq, navTable=navTable)

startDates <- seq.Date(startDate, startDate + 365 * 3, "day")
irr7yr <- sapply(startDates, getSipIrr, sip=sip, numYears=7, 
    freq=freq, navTable=navTable)


getLoadedTable <- function(navTable){    
    st1 <- Sys.time()
    modTable <- navTable[1]
    for (i in c(2:(nrow(navTable) - 1))){
        modTable <- rbind(modTable, navTable[i])
        tdd <- as.numeric(navTable$Date[i + 1] - navTable$Date[i])
        while(tdd > 1){
            modTable <- rbind(modTable, navTable[i + 1])
            modTable[nrow(modTable)]$Date <- modTable[nrow(modTable) - 1]$Date + 1
            tdd <- tdd - 1 
        }
    }    
    modTable <- rbind(modTable, navTable[nrow(navTable)])
    modTable <- modTable[, .(Date, NetAssetValue)]
    st2 <- Sys.time()
    st2 - st1 
    
    return(modTable)
}

getLoadedTableFast <- function(navTable){
    # Function same as getLoadedTable, but twice faster
    # Instead of inserting new rows, creating sequential dates and getting 
    # NAV or close values.
    # getLoadedTable is a self sufficient function, whereas this one uses 
    # getNav function
    st1 <- Sys.time()
    theDates <- seq.Date(navTable[1]$Date, navTable[nrow(navTable)]$Date, by="day")
    modTable <- data.table(Date=theDates, 
        NetAssetValue=sapply(theDates, getNav, navTable = navTable))
    st2 <- Sys.time()
    st2 - st1 
    return(modTable)    
}
    
    
modTable <- getLoadedTableFast(navTable)
    
aa <- modTable$Close
bb <- modTable$Close[1] * (1.00022^(0:(nrow(modTable)-1)))

plot(aa, col="green", type="l")
lines(bb, col="red")

plot(log(aa), col="green", type="l")
lines(log(bb), col="red")

startStr = "01-Sep-1994"
startDate = as.Date(startStr, "%d-%b-%Y") 
numYears = 10
freq = "week"  #month #day #quarter #year
sip = 500
getSipIrr(sip, startDate, numYears, freq, navTable)

compose <- function(f,g){
    return(function(...) f(g(...)))
}

toDouble = compose(as.double, as.character)
toDate = compose(as.Date(..., "%d-%b-%Y"), as.character)

divBy <- function (b){
    if (b == 0) return (0)
    else return (1 / b)
}

isEmpty <- function(x) {
    return(length(x) == 0)
}

getNav <- function(inDate, navDf, keyHeader, valueHeader){
    minDate = navDf[1, ][keyHeader]
    
    maxRow = dim(navDf)[1]
    maxDate = navDf[maxRow, ][keyHeader]
    
    curDate = inDate
    if (curDate < minDate){
        return(0)
        break
    }
    
    dateVector = unlist(navDf[keyHeader])
    rowNum = which(dateVector == curDate)
    while (isEmpty(rowNum)){
        if (curDate > maxDate){
            return(0)
            break
        }
        curDate = curDate + 1
        rowNum = which(dateVector == curDate)
    }
    
    return(unlist(navDf[rowNum[1], ][valueHeader]))
}

getFromAmfi <- function(){
    library(XML)
    # http://portal.amfiindia.com/NavHistoryReport_Frm.aspx
    # UTI Equity Fund Growth 100669
    # UTI Equity Fund Growth (Direct Option) 120662
    statURL <- "http://portal.amfiindia.com/NavHistoryReport_Rpt_Po.aspx?rpt=1&frmdate=18-May-1992&todate=14-Mar-2015&mf=28&scm=100669"
    pageData <- readHTMLTable(statURL)
    namesVec <- as.vector(unlist(pageData[[4]][4,]))
    pageData <- pageData[[4]][-c(1:4), ]
    row.names(pageData) <- NULL
    names(pageData) <- as.vector(unlist(pageData[[4]][4,]))
    
    navTable <- cbind(
                        data.frame(
                            cbind(toDouble(pageData[[1]]), 
                                toDouble(pageData[[2]]),
                                toDouble(pageData[[3]])
                            )
                        ),
                        as.Date(as.character(pageData[[4]]), "%d-%b-%Y")
                    )
    names(navTable) <- namesVec
    # save(navTable, file="filename.RData")
    return (navTable)
}

fvFun <- function(i, cf, n) sum(cf * (1 + i) ^ (n/365)) 

xirr <- function(cfs, redeemDate, invDates) { 
    allDates = c(invDates, redeemDate)
    numDays = as.double(redeemDate - allDates)
    return(uniroot(fvFun, c(-1, 10), cf = cfs, n = numDays)$root) 
}


# navTable <- getFromAmfi()
# save(navTable, file="UTIEQUITYGROWTH-18May1992to14Mar2014.RData")
    
load("navTable.RData")

getIrr <- function(sip, startDate, endDate, freq, redeemDate, navTable, 
                   keyCol, valueCol){
    invDates <- seq.Date(startDate, endDate, freq)
    navs <- sapply(invDates, getNav, navDf = navTable, keyHeader = keyCol,
                    valueHeader = valueCol)
    sips = rep(sip, length(navs))
    invUnits <- sips * unlist(Map(divBy, navs))
    zeroAdjSips = navs * invUnits
    redeemNav <- getNav(redeemDate, navTable, keyCol, valueCol)
    redeemUnits <- sum(invUnits)
    redeemValue <- redeemUnits * redeemNav

    cfs <- c(zeroAdjSips, -redeemValue)
    irrValue = xirr(cfs, redeemDate, invDates)
    return(irrValue)
}


startStr = "10-Jan-2007"
startDate = as.Date(startStr, "%d-%b-%Y") 
numYears = 7
endDate = startDate + numYears * 365
freq = "week"  #month #day #quarter #year
redeemDate = endDate
sip = 500

getIrr(sip, startDate, endDate, freq, redeemDate, bse30df, keyCol = "Date",
    valueCol = "Sensex")

irrs <- sapply(seq.Date(startDate, startDate + 365 * 4, "day"), 
    getIrr, navTable = navTable, numYears = 5)
    